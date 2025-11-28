-module(mcp_bridge_app).

-behaviour(application).

-emqx_plugin(?MODULE).

-export([
    start/2,
    stop/1
]).

-export([
    on_config_changed/2,
    on_health_check/1
]).

%% NOTE
%% Functions from EMQX are unavailable at compile time.
-dialyzer({no_unknown, [start/2, stop/1]}).

start(_StartType, _StartArgs) ->
    {ok, Sup} = mcp_bridge_sup:start_link(),
    mcp_bridge:hook(),
    mcp_bridge_tools:create_table(),
    emqx_ctl:register_command(mcp_bridge, {mcp_bridge_cli, cmd}),
    {ok, _} = start_listener(),
    {ok, Sup}.

stop(_State) ->
    _ = stop_listener(),
    emqx_ctl:unregister_command(mcp_bridge),
    mcp_bridge:unhook().

on_config_changed(OldConfig, NewConfig) ->
    mcp_bridge:on_config_changed(OldConfig, NewConfig).

on_health_check(Options) ->
    mcp_bridge:on_health_check(Options).

start_listener() ->
    #{listening_address := ListeningAddress, certfile := Certfile, keyfile := Keyfile} = mcp_bridge:get_config(),
    #{scheme := Scheme, path := Path, authority := #{port := Port, host := Host}} = ListeningAddress,
    Paths = case Path of
                <<"/sse">> ->
                    mcp_bridge_sse_handler:path_specs();
                _ ->
                    mcp_bridge_sse_handler:path_specs() ++ mcp_bridge_http_handler:path_specs(Path)
            end,
    Dispatch = cowboy_router:compile([
        {'_', Paths}
    ]),
    Middlewares = [mcp_bridge_http_auth, cowboy_router, cowboy_handler],
    case Scheme of
        <<"http">> ->
            cowboy:start_clear(mcp_bridge_http_listener,
                [{port, Port}, {ip, Host}],
                #{env => #{dispatch => Dispatch}, middlewares => Middlewares}
            );
        <<"https">> ->
            SSLOptions = [
                {certfile, Certfile},
                {keyfile, Keyfile}
            ],
            cowboy:start_tls(mcp_bridge_http_listener,
                [{port, Port}, {ip, Host}] ++ SSLOptions,
                #{env => #{dispatch => Dispatch}, middlewares => Middlewares}
            );
        _ -> {error, {invalid_scheme, Scheme}}
    end.

stop_listener() ->
    cowboy:stop_listener(mcp_bridge_http_listener).
