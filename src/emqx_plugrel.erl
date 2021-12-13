-module(emqx_plugrel).

-export([init/1, do/1, format_error/1]).

-define(LOG(LEVEL, FORMAT, ARGS),
        rebar_api:LEVEL("[emqx_plugrel] " ++ FORMAT, ARGS)).

-define(supported_pre_vsns(_CurrVsn), <<".*">>).

-define(PROVIDER, zip).
-define(DEPS, [release]).

-spec init(rebar_state:t()) -> {ok, rebar_state:t()}.
init(State) ->
    Provider = providers:create([
            {namespace, emqx_plugrel},
            {name, ?PROVIDER},            % The 'user friendly' name of the task
            {module, ?MODULE},            % The module implementation of the task
            {bare, true},                 % The task can be run by the user, always true
            {deps, ?DEPS},                % The list of dependencies
            {example, "rebar3 emqx_plugrel zip"}, % How to use the plugin
            {opts, []},                   % list of options understood by the plugin
            {short_desc, "EMQ X plugin zip package"},
            {desc, "A rebar3 plugin that helps to release a zip package for EMQ X plugin"}
    ]),
    {ok, rebar_state:add_provider(State, Provider)}.

do(State) ->
    io:format(user, "~p\n", [State]),
    {ok, State}.

-spec format_error(any()) ->  iolist().
format_error(Reason) ->
    io_lib:format("~p", [Reason]).

