-module(emqx_plugrel).

-export([init/1, do/1, format_error/1]).

-define(METADATA_VSN, <<"0.1.0">>).

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
            {short_desc, "EMQX plugin zip package"},
            {desc, "A rebar3 plugin that helps to release a zip package for EMQX plugin"}
    ]),
    {ok, rebar_state:add_provider(State, Provider)}.

do(State) ->
    Opts = rebar_state:opts(State),
    Relx = rebar_opts:get(Opts, relx),
    %% call rlx_config:to_state/1 to parse the vsns in format of 'git' or '{cmd, Cmd}'
    {ok, RelxState} = rlx_config:to_state(Relx),
    case maps:values(rlx_state:configured_releases(RelxState)) of
        [Rel] ->
            Name = rlx_release:name(Rel),
            Version = rlx_release:vsn(Rel),
            Apps = [App || {App, _} <- rlx_release:app_specs(Rel)],
            PluginInfo = rebar_opts:get(Opts, emqx_plugrel),
            Info = collect_info(PluginInfo, Name, Version, Apps, State),
            ok = make_tar(Info, State);
        [] ->
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
                , git_ref => git_ref()
                , git_commit_or_build_date => get_date()
                , metadata_vsn => ?METADATA_VSN
                , built_on_otp_release => bin(erlang:system_info(otp_release))
                },
    maps:merge(Info, MoreInfo).

%% best-effort deterministic time, read git commit time otherwise now
get_date() ->
    case cmd_oneline_output("git log -1 --pretty=format:'%cd' --date=format:'%Y-%m-%d'") of
        error -> today();
        Date -> Date
    end.

today() ->
    {Y, M, D} = erlang:date(),
    bin(io_lib:format("~4..0b-~2..0b-~2..0b", [Y, M, D])).

git_ref() ->
    case cmd_oneline_output("git rev-parse HEAD") of
        error -> <<"unknown">>;
        Ref -> Ref
    end.

cmd_oneline_output(Cmd) ->
    case rebar_utils:sh(Cmd, [{use_stdout, false}, return_on_error]) of
        {ok, Line} ->
            bin(rebar_string:trim(Line, trailing, "\n"));
        {error, {Rc, Output}} ->
            ?LOG(debug, "failed_run_cmd ~s~n, error=~p~noutput:~n~ts~n", [Rc, Output]),
            error
    end.

%% Find app vsn from compiled .app files
%% such info is technically available from within rebar State,
%% however that requires some deep knowledge of rebar3 internals
%% Returns a list of app names with -<vsn> suffix in binary() string format.
resolve_vsn(App, State) ->
    BaseDir = rebar_dir:base_dir(State),
    AppStr = atom_to_list(App),
    AppFile = filename:join([BaseDir, "lib", App, "ebin", bin([AppStr, ".app"])]),
    case file:consult(AppFile) of
        {ok, AppInfo} ->
            bin(AppStr ++ "-" ++ get_vsn(AppInfo));
        {error, Reason} ->
            ?LOG(error, "failed_to_read_app_vsn ~ts: ~p", [AppFile, Reason]),
            error({failed_to_read_app_vsn, AppFile, Reason})
    end.

get_vsn([{application, _Name, Info}]) ->
    {vsn, Vsn} = lists:keyfind(vsn, 1, Info),
    Vsn.

make_tar(#{name := Name, rel_vsn := Vsn, rel_apps := Apps} = Info, State) ->
    BaseDir = rebar_dir:base_dir(State),
    Dir = filename:join([BaseDir, ?MODULE]),
    NameWithVsn = binary_to_list(bin([Name, "-", Vsn])),
    LibDir = filename:join([Dir, NameWithVsn]),
    InfoFile = filename:join([LibDir, "release" ++ ".json"]),
    %% ensure clean state
    ok = rebar_file_utils:rm_rf(LibDir),
    ok = filelib:ensure_dir(InfoFile),
    %% copy README.md if present
    case file:read_file_info("README.md") of
        {ok, _} -> rebar_file_utils:cp_r(["README.md"], LibDir);
        _ -> ok
    end,
    %% write info file
    ok = file:write_file(InfoFile, jsx:encode(Info, [space, {indent, 4}])),
    %% copy apps to lib dir
    Sources = lists:map(fun(App) -> filename:join([BaseDir, "rel", Name, "lib", App]) end, Apps),
    ok = rebar_file_utils:cp_r(Sources, LibDir),
    {ok, OriginalCwd} = file:get_cwd(),
    ok = file:set_cwd(Dir),
    try
        do_make_tar(Dir, NameWithVsn)
    after
        file:set_cwd(OriginalCwd)
    end.

do_make_tar(Cwd, NameWithVsn) ->
    Files = filelib:wildcard(NameWithVsn ++ "/**"),
    TarFile = NameWithVsn ++ ".tar.gz",
    FullName = filename:join([Cwd, TarFile]),
    ?LOG(info, "creating ~ts", [FullName]),
    ok = erl_tar:create(TarFile, Files, [compressed]),
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

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
date_test() ->
    ?assertMatch([$2, $0, _, _, $-, _, _, $-, _, _], binary_to_list(today())),
    ?assertMatch([$2, $0, _, _, $-, _, _, $-, _, _], binary_to_list(get_date())).
-endif.
