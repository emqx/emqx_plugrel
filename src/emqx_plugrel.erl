-module(emqx_plugrel).

-export([init/1, do/1, format_error/1]).

-define(LOG(LEVEL, FORMAT, ARGS),
        rebar_api:LEVEL("[emqx_plugrel] " ++ FORMAT, ARGS)).

-spec init(rebar_state:t()) -> {ok, rebar_state:t()}.
init(State) ->
    Provider = providers:create([
            {namespace, emqx_plugrel},
            {name, tar}, % The 'user friendly' name of the task
            {module, ?MODULE}, % The module implementation of the task
            {bare, true}, % The task can be run by the user, always true
            {deps, [{default, release}]},  % The list of dependencies
            {example, "rebar3 emqx_plugrel tar"}, % How to use the plugin
            {opts, [emqx_plugrel]}, % list of options understood by the plugin
            {short_desc, "EMQ X plugin zip package"},
            {desc, "A rebar3 plugin that helps to release a zip package for EMQ X plugin"}
    ]),
    {ok, rebar_state:add_provider(State, Provider)}.

do(State) ->
    Opts = rebar_state:opts(State),
    Relx = rebar_opts:get(Opts, relx),
    PluginInfo = rebar_opts:get(Opts, emqx_plugrel),
    case lists:keyfind(release, 1, Relx) of
        {release, {Name, Version}, Apps} ->
            Info = collect_info(PluginInfo, Name, Version, Apps, State),
            ok = make_tar(Info);
        false ->
            ?LOG(error, "relx_config_not_found", []),
            error(relx_config_not_found)
    end,
    {ok, State}.

-spec format_error(any()) ->  iolist().
format_error(Reason) ->
    io_lib:format("~p", [Reason]).

collect_info(PluginInfo, Name, Version, Apps, State) ->
    AppsWithVsn = lists:map(fun(App) -> resolve_vsn(App, State) end, Apps),
    Info = info_map(PluginInfo),
    MoreInfo = #{ name => bin(atom_to_list(Name))
                , rel_vsn => bin(Version)
                , rel_apps => AppsWithVsn
                , build_time => now_time()
                },
    maps:merge(Info, MoreInfo).

now_time() ->
    bin(calendar:system_time_to_rfc3339(erlang:system_time(second))).

%% Find app vsn from compiled .app files
%% such info is technically available from within rebar State,
%% however that requires some deep knowledge of rebar3 internals
%% Returns a list of app names with -<vsn> suffix in binary() string format.
resolve_vsn(App, _State) ->
    AppStr = atom_to_list(App),
    AppFile = filename:join(["_build", "default", "lib", App, "ebin", bin([AppStr, ".app"])]),
    case file:consult(AppFile) of
        {ok, AppInfo} ->
            bin(AppStr ++ "-" ++ get_vsn(AppInfo));
        {error, Reason} ->
            ?LOG(error, "failed_to_read_app_vsn ~s ~p", [AppFile, Reason]),
            error({failed_to_read_app_vsn, AppFile, Reason})
    end.

get_vsn([{application, _Name, Info}]) ->
    {vsn, Vsn} = lists:keyfind(vsn, 1, Info),
    Vsn.

make_tar(#{name := Name, rel_vsn := Vsn, rel_apps := Apps} = Info) ->
    Dir = filename:join(["_build", ?MODULE]),
    NameWithVsn = binary_to_list(bin([Name, "-", Vsn])),
    %% write info file
    InfoFile = filename:join([Dir, NameWithVsn ++ ".json"]),
    ok = filelib:ensure_dir(InfoFile),
    ok = file:write_file(InfoFile, jsx:encode(Info, [space, {indent, 4}])),
    %% copy apps to lib dir
    LibDir = filename:join([Dir, lib]),
    ok = rebar_file_utils:rm_rf(LibDir),
    ok = filelib:ensure_dir(filename:join([LibDir, "foo"])),
    Sources = lists:map(fun(App) -> filename:join(["_build", "default", "rel", Name, "lib", App]) end, Apps),
    ok = rebar_file_utils:cp_r(Sources, LibDir),
    {ok, OriginalCwd} = file:get_cwd(),
    ok = file:set_cwd(Dir),
    try
        do_make_tar(Dir, NameWithVsn)
    after
        file:set_cwd(OriginalCwd)
    end,
    ok = file:delete(InfoFile).

do_make_tar(Cwd, NameWithVsn) ->
    Files = filelib:wildcard("lib/**"),
    TarFile = NameWithVsn ++ ".tar.gz",
    FullName = filename:join([Cwd, TarFile]),
    ?LOG(info, "creating ~s", [FullName]),
    ok = erl_tar:create(TarFile, [NameWithVsn ++ ".json"| Files], [compressed]),
    {ok, Bin} = file:read_file(TarFile),
    Sha = bin2hexstr(crypto:hash(sha256, Bin)),
    ok = file:write_file(NameWithVsn ++ ".sha256", Sha).

bin(X) -> iolist_to_binary(X).

str_list(L) -> lists:map(fun bin/1, L).

info_field(authors, Authors) -> str_list(Authors);
info_field(builder, Builder) -> info_map(Builder);
info_field(functionality, Fs) -> str_list(Fs);
info_field(compatibility, Cs) -> info_map(Cs);
info_field(_, Value) -> bin(Value).

info_map(InfoList) ->
    maps:from_list(lists:map(fun({K, V}) -> {K, info_field(K, V)} end, InfoList)).

bin2hexstr(B) when is_binary(B) ->
    << <<(int2hexchar(H)), (int2hexchar(L))>> || <<H:4, L:4>> <= B>>.

int2hexchar(I) when I >= 0 andalso I < 10 -> I + $0;
int2hexchar(I) -> I - 10 + $a.