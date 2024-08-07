-module(emqx_plugrel).

-feature(maybe_expr, enable).

-export([init/1, do/1, format_error/1]).

-define(METADATA_VSN, <<"0.2.0">>).
-define(PLUG_REL_TAG, <<"0.5.1">>).

-define(plugin_readme_file, "README.md").
-define(plugin_avsc_file, "priv/config_schema.avsc").
-define(plugin_i18n_file, "priv/config_i18n.json").
-define(plugin_hocon_file, "priv/config.hocon").
-define(validate_but_no_copy, undefined).


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
            Apps = [App || {App, _} <- rlx_release:goals(Rel)],
            PluginInfo = rebar_opts:get(Opts, emqx_plugrel),
            Info = collect_info(PluginInfo, Name, Version, Apps, State),
            ok = make_tar(Info, State);
        [] ->
            ?LOG(error, "relx_config_not_found", []),
            error(relx_config_not_found)
    end,
    {ok, State}.

-spec format_error(any()) -> iolist().
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
                , with_config_schema => filelib:is_regular(?plugin_avsc_file)
                , emqx_plugrel_vsn => ?PLUG_REL_TAG
                , hidden => maps:get(hidden, Info, false)
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
            ?LOG(debug, "failed_run_cmd ~s~n, error=~p~noutput:~n~ts~n", [Cmd, Rc, Output]),
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
    ?LOG(info, "Pre-processing to create tarball for ~ts-~ts", [Name, Vsn]),
    BaseDir = rebar_dir:base_dir(State),
    Dir = filename:join([BaseDir, ?MODULE]),
    NameWithVsn = binary_to_list(bin([Name, "-", Vsn])),
    LibDir = filename:join([Dir, NameWithVsn]),
    InfoFile = filename:join([LibDir, "release.json"]),
    %% ensure clean state
    ok = rebar_file_utils:rm_rf(LibDir),
    ok = filelib:ensure_dir(InfoFile),
    ok = maybe_copy_files(LibDir),
    %% write info file
    ok = file:write_file(InfoFile, jsone:encode(Info, [native_forward_slash, {space, 1}, {indent, 4}, native_utf8])),
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
    Files = filelib:wildcard(NameWithVsn ++ "/*"),
    TarFile = NameWithVsn ++ ".tar.gz",
    FullName = filename:join([Cwd, TarFile]),
    ?LOG(info, "Trying create ~ts", [FullName]),
    ok = erl_tar:create(TarFile, Files, [compressed]),
    {ok, Bin} = file:read_file(TarFile),
    Sha = bin2hexstr(crypto:hash(sha256, Bin)),
    ok = file:write_file(NameWithVsn ++ ".sha256", Sha).

maybe_copy_files(LibDir) ->
    lists:foreach(
        fun({F, TargetDir}) ->
            maybe
                true ?= filelib:is_regular(F),
                true ?= validate_file(F), %% validate only when file existed
                case TargetDir of
                    ?validate_but_no_copy -> ok;
                    _ -> rebar_file_utils:cp_r([F], TargetDir), ok
                end
            else
                _ -> ok
            end
        end,
        [ {?plugin_readme_file, LibDir}
        , {?plugin_avsc_file, ?validate_but_no_copy}
        , {?plugin_i18n_file, ?validate_but_no_copy}
        ]
    ),
    ok.

validate_file(?plugin_avsc_file = F) ->
    validate_avsc(F);
validate_file(?plugin_i18n_file = F) ->
    validate_i18n(F);
validate_file(F) ->
    ?LOG(debug, "Skipping validate file ~ts", [F]),
    true.

validate_avsc(F) ->
    Name = <<"TryDecodeDefaultAvro">>,
    try
        begin
            {ok, HoconMap} = hocon:load(?plugin_hocon_file),
            JsonEncodedAvroBin = jsone:encode(HoconMap, [native_forward_slash, {space, 1}, {indent, 4}]),
            Store0 = avro_schema_store:new([map]),
            {ok, AvscBin} = file:read_file(?plugin_avsc_file),
            Store = avro_schema_store:import_schema_json(Name, AvscBin, Store0),
            Opts = avro:make_decoder_options([{map_type, map}, {record_type, map}, {encoding, avro_json}]),
            {ok, avro_json_decoder:decode_value(JsonEncodedAvroBin, Name, Store, Opts)}
        end

    of
        {ok, _Value} ->
            ?LOG(debug, "Schema matched. Valid Plugin Hocon and Schema files: ~ts, ~ts", [?plugin_hocon_file, ?plugin_avsc_file]),
            true
    catch
        E : R : S ->
            ?LOG(error, "Invalid plugin Hocon or Schema. Please check ~ts and ~ts.~n"
                        "Error = ~p, Reason = ~p, Stacktrace=~p",
                        [?plugin_hocon_file, ?plugin_avsc_file, E, R, S]),
            error({failed_to_validate_avsc_file, F})
    end.

validate_i18n(F) ->
    {ok, Bin} = file:read_file(F),
    try jsone:decode(Bin, [{object_format, map}]) of
        _ ->
            ?LOG(debug, "Valid i18n file: ~ts", [F]),
            true
    catch
        E : R : S ->
            ?LOG(error, "Invalid i18n json file. Error = ~p, Reason = ~p, Stacktrace=~p", [E, R, S]),
            error({failed_to_validate_i18n_file, F})
    end.

bin(X) -> unicode:characters_to_binary(X, utf8).

info_field(compatibility, Cs) -> info_map(Cs);
info_field(builder, Builder) -> info_map(Builder);
info_field(authors, Authors) -> bin(Authors);
info_field(functionality, Fs) -> bin(Fs);
info_field(hidden, Bool) when is_boolean(Bool) -> Bool;
info_field(_K, Value) -> bin(Value).

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
