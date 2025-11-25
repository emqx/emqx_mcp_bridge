-module(mcp_bridge_message).

-include("mcp_bridge.hrl").

-export([ initialize_request/2
        , initialize_request/3
        , initialized_notification/0
        , list_tools_request/1
        ]).

-export([ json_rpc_request/3
        , json_rpc_response/2
        , json_rpc_notification/1
        , json_rpc_notification/2
        , json_rpc_error/4
        , decode_rpc_msg/1
        , get_topic/2
        , make_mqtt_msg/5
        , publish_mqtt_msg/2
        ]).

%%==============================================================================
%% MCP Requests/Responses/Notifications
%%==============================================================================
initialize_request(ClientInfo, Capabilities) ->
    initialize_request(1, ClientInfo, Capabilities).

initialize_request(Id, ClientInfo, Capabilities) ->
    json_rpc_request(
        Id,
        <<"initialize">>,
        #{
            <<"protocolVersion">> => ?MCP_VERSION,
            <<"clientInfo">> => ClientInfo,
            <<"capabilities">> => Capabilities
        }
    ).

initialized_notification() ->
    json_rpc_notification(<<"notifications/initialized">>).

list_tools_request(Id) ->
    json_rpc_request(
        Id,
        <<"tools/list">>,
        #{}
    ).

%%==============================================================================
%% JSON RPC Messages
%%==============================================================================
json_rpc_request(Id, Method, Params) ->
    emqx_utils_json:encode(#{
        <<"jsonrpc">> => <<"2.0">>,
        <<"method">> => Method,
        <<"params">> => Params,
        <<"id">> => Id
    }).

json_rpc_response(Id, Result) ->
    emqx_utils_json:encode(#{
        <<"jsonrpc">> => <<"2.0">>,
        <<"result">> => Result,
        <<"id">> => Id
    }).

json_rpc_notification(Method) ->
    emqx_utils_json:encode(#{
        <<"jsonrpc">> => <<"2.0">>,
        <<"method">> => Method
    }).

json_rpc_notification(Method, Params) ->
    emqx_utils_json:encode(#{
        <<"jsonrpc">> => <<"2.0">>,
        <<"method">> => Method,
        <<"params">> => Params
    }).

json_rpc_error(Id, Code, Message, Data) ->
    emqx_utils_json:encode(#{
        <<"jsonrpc">> => <<"2.0">>,
        <<"error">> => #{
            <<"code">> => Code,
            <<"message">> => Message,
            <<"data">> => Data
        },
        <<"id">> => Id
    }).

decode_rpc_msg(Msg) ->
    try emqx_utils_json:decode(Msg) of
        #{
            <<"jsonrpc">> := <<"2.0">>,
            <<"method">> := Method,
            <<"params">> := Params,
            <<"id">> := Id
        } ->
            {ok, #{type => json_rpc_request, method => Method, id => Id, params => Params}};
        #{<<"jsonrpc">> := <<"2.0">>, <<"result">> := Result, <<"id">> := Id} ->
            {ok, #{type => json_rpc_response, id => Id, result => Result}};
        #{<<"jsonrpc">> := <<"2.0">>, <<"error">> := Error, <<"id">> := Id} ->
            {ok, #{type => json_rpc_error, id => Id, error => Error}};
        #{<<"jsonrpc">> := <<"2.0">>, <<"method">> := Method, <<"params">> := Params} ->
            {ok, #{type => json_rpc_notification, method => Method, params => Params}};
        #{<<"jsonrpc">> := <<"2.0">>, <<"method">> := Method} ->
            {ok, #{type => json_rpc_notification, method => Method}};
        Msg1 ->
            {error, #{reason => malformed_json_rpc, msg => Msg1}}
    catch
        error:Reason ->
            {error, #{reason => invalid_json, msg => Msg, details => Reason}}
    end.

get_topic(server_control, #{server_id := ServerId, server_name := ServerName}) ->
    <<"$mcp-server/", ServerId/binary, "/", ServerName/binary>>;
get_topic(server_capability_list_changed, #{server_id := ServerId, server_name := ServerName}) ->
    <<"$mcp-server/capability/", ServerId/binary, "/", ServerName/binary>>;
get_topic(server_resources_updated, #{server_id := ServerId, server_name := ServerName}) ->
    <<"$mcp-server/capability/", ServerId/binary, "/", ServerName/binary>>;
get_topic(server_presence, #{server_id := ServerId, server_name := ServerName}) ->
    <<"$mcp-server/presence/", ServerId/binary, "/", ServerName/binary>>;
get_topic(client_presence, #{mcp_clientid := McpClientId}) ->
    <<"$mcp-client/presence/", McpClientId/binary>>;
get_topic(client_capability_list_changed, #{mcp_clientid := McpClientId}) ->
    <<"$mcp-client/capability/", McpClientId/binary>>;
get_topic(rpc, #{mcp_clientid := McpClientId, server_id := ServerId, server_name := ServerName}) ->
    <<"$mcp-rpc/", McpClientId/binary, "/", ServerId/binary, "/", ServerName/binary>>.

make_mqtt_msg(Topic, Payload, McpClientId, Flags, QoS) ->
    UserProps = [
        {<<"MCP-COMPONENT-TYPE">>, <<"mcp-client">>},
        {<<"MCP-MQTT-CLIENT-ID">>, McpClientId}
    ],
    Headers = #{
        properties => #{
            'User-Property' => UserProps
        }
    },
    QoS = 1,
    emqx_message:make(McpClientId, QoS, Topic, Payload, Flags, Headers).

publish_mqtt_msg(ClientId, Msg) ->
    case emqx_cm:lookup_channels(ClientId) of
        [] ->
            {error, session_not_found};
        Pids when is_list(Pids) ->
            erlang:send(lists:last(Pids), {deliver, emqx_message:topic(Msg), Msg}),
            ok
    end.
