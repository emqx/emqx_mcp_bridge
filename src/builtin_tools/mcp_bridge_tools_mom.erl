-module(mcp_bridge_tools_mom).

%% Callbacks for MOM (MCP over MQTT), can be useful when the use wants to handle
%% user specific logic when calling tools.

-include("mcp_bridge.hrl").

-export([
    on_tools_call/5
]).

on_tools_call(_ToolType, _ToolName, Topic, McpRequest, _Meta) ->
    %% Here we can modify the Topic or McpRequest if needed
    {ok, Topic, McpRequest}.
