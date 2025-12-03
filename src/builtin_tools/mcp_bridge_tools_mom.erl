-module(mcp_bridge_tools_mom).

%% Callbacks for MOM (MCP over MQTT), can be useful when the use wants to handle
%% user specific logic when calling tools.

-include("mcp_bridge.hrl").

-export([
    on_tools_call/2
]).

on_tools_call(MqttClientId, #{params := Params} = McpRequest) ->
    Name = maps:get(<<"name">>, Params, <<>>),
    case string:split(Name, ":") of
        [ToolType, ToolName] ->
            %% offload the target client id param if exists
            Params1 = maps:remove(?TARGET_CLIENTID_KEY, Params),
            %% offload the tool type from the tool name
            McpRequest1 = McpRequest#{params := Params1#{<<"name">> => ToolName}},
            Topic = mcp_bridge_topics:get_topic(rpc, #{
                mcp_clientid => ?MCP_CLIENTID_B,
                server_id => MqttClientId,
                server_name => ToolType
            }),
            {ok, Topic, McpRequest1};
        _ ->
            {error, #{
                reason => invalid_tool_name,
                name => Name,
                details => <<"Tool name must be in format 'tool_type:tool_name'">>
            }}
    end.
