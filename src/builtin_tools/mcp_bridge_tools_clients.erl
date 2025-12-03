-module(mcp_bridge_tools_clients).

-include("mcp_bridge.hrl").

-export([
    get_client_status/2,
    publish_mqtt_message/2
]).

%% Tool type is used to categorize the tools. A module can define only one tool type.
%% Here we define a tool type "emqx/clients" which means all tools defined in this module
%% are related to EMQX clients management.
-tool_type(<<"emqx/clients">>).

-tool(#{
    %% Note that the 'name' field must be the callback function name defined below.
    name => get_client_status,
    title => <<"Get Client Status">>,
    description => <<""
    "Get the status of a given MQTT client ID. "
    "Returns detailed client session information like if the client is connected, "
    "its IP address/port and other metadata."
    "">>,
    inputSchema => #{
        type => object,
        properties => #{
            client_id => #{
                type => string,
                description => <<"The MQTT client ID to query">>
            }
        },
        required => [client_id]
    },
    opts => #{
        %% Optional tool-specific options can be defined here.
        %% When the tool is called, these options will be passed to the tool function
        %% via the 'opts' key in the Meta parameter.
        verbose => 1
    }
}).

-tool(#{
    name => publish_mqtt_message,
    title => <<"Publish MQTT Message">>,
    description => <<""
    "Publish an MQTT message to a specific topic. "
    "This tool allows sending messages as if they were published by the target client."
    "">>,
    inputSchema => #{
        type => object,
        properties => #{
            topic => #{
                type => string,
                description => <<"The MQTT topic to publish the message to">>
            },
            payload => #{
                type => string,
                description => <<"The message payload">>
            },
            qos => #{
                type => integer,
                description => <<"The QoS level for the message (0, 1, or 2)">>,
                enum => [0, 1, 2],
                default => 0
            },
            retain => #{
                type => boolean,
                description => <<"Whether the message should be retained by the broker">>,
                default => false
            },
            payload_encoding => #{
                type => string,
                description => <<"The encoding of the payload, (plain or base64)">>,
                default => <<"plain">>,
                enum => [<<"plain">>, <<"base64">>]
            }
        },
        required => [topic, payload]
    },
    opts => #{}
}).

-spec get_client_status(request_params(), request_meta()) -> {ok, response()} | {error, term()}.
get_client_status(#{<<"client_id">> := ClientId}, #{opts := Opts} = _Meta) ->
    %% this is an example tool that returns the status of a given MQTT clientid
    case emqx_mgmt_api_clients:client(get, #{bindings => #{clientid => ClientId}}) of
        {200, ClientInfo} ->
            {ok, format_clientinfo(ClientInfo, Opts)};
        {404, _} ->
            {error, session_not_found}
    end.

-spec publish_mqtt_message(request_params(), request_meta()) -> {ok, term()} | {error, term()}.
publish_mqtt_message(Params, #{opts := _Opts} = _Meta) ->
    %% this is an example tool that publishes an MQTT message on behalf of a target clientid
    case emqx_mgmt_api_publish:publish(post, #{body => Params}) of
        {200, Response} -> {ok, Response};
        {_Code, Response} -> {error, Response}
    end.

%%==============================================================================
%% Internal functions
%%==============================================================================
-define(CK1, [
    clientid, connected, clean_start, keepalive, ip_address, port,
    connected_at, node, is_expired, created_at,
    expiry_interval, proto_name, proto_ver
]).
-define(CK2, [subscriptions_cnt, mqueue_len, inflight_cnt, heap_size, enable_authn, listener, durable] ++ ?CK1).
format_clientinfo(ClientInfo, #{verbose := N}) when N >= 3 ->
    %% If verbose is true, include more details
    ClientInfo;
format_clientinfo(ClientInfo, #{verbose := 2}) ->
    %% If verbose is false, return a simplified version
    maps:with(?CK2, ClientInfo);
format_clientinfo(ClientInfo, #{verbose := 1}) ->
    maps:with(?CK1, ClientInfo).
