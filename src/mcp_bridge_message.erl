-module(mcp_bridge_message).

-include("mcp_bridge.hrl").
-include_lib("emqx_plugin_helper/include/emqx.hrl").

-export([
    initialize_request/2,
    initialize_request/3,
    initialize_response/3,
    initialize_response/4,
    initialized_notification/0,
    list_tools_request/1
]).

-export([
    get_tools_list/3,
    send_tools_call/5,
    send_mcp_request/5
]).

-export([
    json_rpc_request/3,
    json_rpc_response/2,
    json_rpc_notification/1,
    json_rpc_notification/2,
    json_rpc_error/4,
    decode_rpc_msg/1,
    make_mqtt_msg/5,
    reply_caller/2,
    complete_mqtt_msg/2
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

initialize_response(Id, ServerInfo, Capabilities) ->
    json_rpc_response(Id, #{
        <<"protocolVersion">> => ?MCP_VERSION,
        <<"serverInfo">> => ServerInfo,
        <<"capabilities">> => Capabilities
    }).

initialize_response(Id, ServerInfo, Capabilities, Instructions) ->
    json_rpc_response(Id, #{
        <<"protocolVersion">> => ?MCP_VERSION,
        <<"serverInfo">> => ServerInfo,
        <<"capabilities">> => Capabilities,
        <<"instructions">> => Instructions
    }).

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
        #{<<"jsonrpc">> := <<"2.0">>, <<"method">> := Method, <<"id">> := Id} = RpcMsg ->
            Params = maps:get(<<"params">>, RpcMsg, #{}),
            {ok, #{type => json_rpc_request, method => Method, id => Id, params => Params}};
        #{<<"jsonrpc">> := <<"2.0">>, <<"result">> := Result, <<"id">> := Id} ->
            {ok, #{type => json_rpc_response, id => Id, result => Result}};
        #{<<"jsonrpc">> := <<"2.0">>, <<"error">> := Error, <<"id">> := Id} ->
            {ok, #{type => json_rpc_error, id => Id, error => Error}};
        #{<<"jsonrpc">> := <<"2.0">>, <<"method">> := Method} = RpcMsg ->
            Params = maps:get(<<"params">>, RpcMsg, #{}),
            {ok, #{type => json_rpc_notification, method => Method, params => Params}};
        Msg1 ->
            {error, #{reason => malformed_json_rpc, msg => Msg1}}
    catch
        error:Reason ->
            {error, #{reason => invalid_json, msg => Msg, details => Reason}}
    end.

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

get_tools_list(Headers, JwtClaims, McpReqId) ->
    case get_tools_types(Headers, JwtClaims) of
        {ok, ToolTypes} ->
            list_tools_result(mcp_bridge_tool_registry:list_tools(ToolTypes), McpReqId);
        {error, _} ->
            list_tools_result(mcp_bridge_tool_registry:list_tools(), McpReqId)
    end.

send_tools_call(
    HttpHeaders,
    JwtClaims,
    #{method := <<"tools/call">>, id := McpReqId, params := Params} = McpRequest,
    WaitResponse,
    Timeout
) ->
    case get_target_clientid(HttpHeaders, JwtClaims, Params) of
        {error, Reason} ->
            call_tool_result({error, Reason}, McpReqId);
        MqttClientId ->
            Result =
                case mcp_bridge_tools_mom:on_tools_call(MqttClientId, McpRequest) of
                    {ok, Topic, McpRequest1} ->
                        send_mcp_request(MqttClientId, Topic, McpRequest1, WaitResponse, Timeout);
                    {error, Reason} ->
                        {error, Reason}
                end,
            call_tool_result(Result, McpReqId)
    end.

get_tools_types(Headers, JwtClaims) ->
    case mcp_bridge:get_config() of
        #{get_tool_types_from := <<"http_headers">>} ->
            case maps:get(?TOOL_TYPES_KEY, Headers, undefined) of
                undefined ->
                    {error, not_found};
                ToolTypes ->
                    case string:lexemes(ToolTypes, ", ") of
                        [] ->
                            {error, no_tool_types_specified};
                        TypesList ->
                            {ok, TypesList}
                    end
            end;
        #{get_tool_types_from := <<"jwt_claims">>} ->
            case maps:get(?TOOL_TYPES_KEY, JwtClaims, undefined) of
                undefined ->
                    {error, not_found};
                ToolTypes when is_list(ToolTypes) ->
                    {ok, ToolTypes};
                _Other ->
                    {error, invalid_tool_types_format}
            end
    end.

get_target_clientid(_, _, #{<<"arguments">> := #{?TARGET_CLIENTID_KEY := MqttClientId}}) ->
    MqttClientId;
get_target_clientid(HttpHeaders, JwtClaims, _Params) ->
    case mcp_bridge:get_config() of
        #{get_target_clientid_from := <<"tool_params">>} ->
            {error, <<?TARGET_CLIENTID_KEY_S " not found in tool params">>};
        #{get_target_clientid_from := <<"http_headers">>} ->
            maps:get(
                ?TARGET_CLIENTID_KEY,
                HttpHeaders,
                {error, <<?TARGET_CLIENTID_KEY_S " not found in http headers">>}
            );
        #{get_target_clientid_from := <<"jwt_claims">>} ->
            maps:get(
                ?TARGET_CLIENTID_KEY,
                JwtClaims,
                {error, <<?TARGET_CLIENTID_KEY_S " not found in jwt claims">>}
            )
    end.

send_mcp_request(MqttClientId, Topic, McpRequest, WaitResponse, Timeout) ->
    case emqx_cm:lookup_channels(MqttClientId) of
        [] ->
            {error, session_not_found};
        Pids when is_list(Pids) ->
            Pid = lists:last(Pids),
            Mref = erlang:monitor(process, Pid),
            Msg1 = make_semi_finished_mqtt_msg(Topic, Mref, McpRequest, WaitResponse),
            erlang:send(Pid, {deliver, emqx_message:topic(Msg1), Msg1}, [noconnect]),
            receive
                {mcp_response, Mref, Reply} ->
                    erlang:demonitor(Mref, [flush]),
                    {ok, Reply};
                {'DOWN', Mref, _, _, noconnection} ->
                    {error, #{reason => nodedown, node => node(Pid)}};
                {'DOWN', Mref, _, _, Reason} ->
                    {error, #{reason => session_die, detail => Reason}}
            after Timeout ->
                erlang:demonitor(Mref, [flush]),
                receive
                    {mcp_response, Reply} ->
                        {ok, Reply}
                after 0 ->
                    {error, timeout}
                end
            end
    end.

reply_caller(#{caller := Caller, monitor_ref := Mref}, Reply) ->
    Caller ! {mcp_response, Mref, Reply},
    ok.

list_tools_result(Tools, McpReqId) ->
    json_rpc_response(McpReqId, #{
        tools => Tools
    }).

call_tool_result({ok, Reply}, McpReqId) when is_atom(Reply) ->
    json_rpc_response(McpReqId, #{
        <<"isError">> => false,
        <<"content">> => [
            #{
                <<"type">> => <<"text">>,
                <<"text">> => Reply
            }
        ]
    });
call_tool_result({ok, Reply}, McpReqId) ->
    json_rpc_response(McpReqId, Reply);
call_tool_result({error, Reason}, McpReqId) ->
    json_rpc_response(McpReqId, #{
        <<"isError">> => true,
        <<"content">> => [
            #{
                <<"type">> => <<"text">>,
                <<"text">> => format_reason(Reason)
            }
        ]
    }).

format_reason(Reason) when is_binary(Reason) ->
    Reason;
format_reason(Reason) when is_atom(Reason) ->
    atom_to_binary(Reason, utf8);
format_reason(Reason) ->
    iolist_to_binary(io_lib:format("~p", [Reason])).

make_mcp_msg_header(Mref, McpRequest, WaitResponse) ->
    #{
        caller => self(),
        monitor_ref => Mref,
        mcp_request => McpRequest,
        wait_response => WaitResponse,
        timestamp => erlang:system_time(millisecond)
    }.

make_semi_finished_mqtt_msg(Topic, Mref, McpRequest, WaitResponse) ->
    %% Set an empty payload and put the MCP request into message header
    Msg = make_mqtt_msg(Topic, <<>>, ?MCP_CLIENTID_B, #{}, 1),
    emqx_message:set_header(?MCP_MSG_HEADER, make_mcp_msg_header(Mref, McpRequest, WaitResponse), Msg).

complete_mqtt_msg(
    #message{headers = #{?MCP_MSG_HEADER := McpMsgHeader} = Headers} = Message, MqttId
) ->
    #{mcp_request := #{method := Method, params := Params}} = McpMsgHeader,
    %% replace the request id with MQTT message id to avoid conflict
    Payload = json_rpc_request(MqttId, Method, Params),
    Message#message{payload = Payload, headers = maps:remove(?MCP_MSG_HEADER, Headers)}.
