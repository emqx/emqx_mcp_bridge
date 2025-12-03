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
    emqx_ctl:register_command(mcp_bridge, {mcp_bridge_cli, cmd}),
    {ok, _} = mcp_bridge:start_listener(),
    {ok, Sup}.

stop(_State) ->
    _ = mcp_bridge:stop_listener(),
    emqx_ctl:unregister_command(mcp_bridge),
    mcp_bridge:unhook().

on_config_changed(OldConfig, NewConfig) ->
    mcp_bridge:on_config_changed(OldConfig, NewConfig).

on_health_check(Options) ->
    mcp_bridge:on_health_check(Options).
