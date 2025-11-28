-module(mcp_bridge).

-include("mcp_bridge.hrl").

%% for #message{} record
%% no need for this include if we call emqx_message:to_map/1 to convert it to a map
-include_lib("emqx_plugin_helper/include/emqx.hrl").

%% for hook priority constants
-include_lib("emqx_plugin_helper/include/emqx_hooks.hrl").

%% for logging
-include_lib("emqx_plugin_helper/include/logger.hrl").

-export([
    hook/0,
    unhook/0,
    start_link/0
]).

-export([
    start_listener/0,
    stop_listener/0,
    restart_listener/0
]).

-export([
    on_config_changed/2,
    on_health_check/1,
    get_config/0
]).

%% Hook callbacks
-export([
    on_client_connected/2,
    on_message_publish/1,
    on_message_delivered/2,
    on_message_puback/4
]).

-export([
    init/1,
    handle_call/3,
    handle_cast/2,
    handle_info/2,
    terminate/2
]).

-define(PROP_K_MCP_COMP_TYPE, <<"MCP-COMPONENT-TYPE">>).
-define(PROP_K_MCP_SERVER_NAME, <<"MCP-SERVER-NAME">>).
-define(INIT_REQ_ID, <<"init_1">>).
-define(LIST_TOOLS_REQ_ID, <<"list_tools_1">>).

%% NOTE
%% Functions from EMQX are unavailable at compile time.
-dialyzer({no_unknown, [hook/0, unhook/0, on_message_puback/4]}).

%% @doc
%% Called when the plugin application start
hook() ->
    emqx_hooks:add('client.connected', {?MODULE, on_client_connected, []}, ?HP_LOWEST),
    emqx_hooks:add('message.publish', {?MODULE, on_message_publish, []}, ?HP_HIGHEST),
    emqx_hooks:add('message.delivered', {?MODULE, on_message_delivered, []}, ?HP_HIGHEST),
    emqx_hooks:add('message.puback', {?MODULE, on_message_puback, []}, ?HP_HIGHEST).

%% @doc
%% Called when the plugin stops
unhook() ->
    emqx_hooks:del('client.connected', {?MODULE, on_client_connected}),
    emqx_hooks:del('message.publish', {?MODULE, on_message_publish}),
    emqx_hooks:del('message.delivered', {?MODULE, on_message_delivered}),
    emqx_hooks:del('message.puback', {?MODULE, on_message_puback}).

%%--------------------------------------------------------------------
%% Hook callbacks
%%--------------------------------------------------------------------
on_client_connected(_ClientInfo, ConnInfo) ->
    UserPropsConn = maps:get('User-Property', maps:get(conn_props, ConnInfo, #{}), []),
    case proplists:get_value(?PROP_K_MCP_COMP_TYPE, UserPropsConn) of
        <<"mcp-server">> ->
            erlang:put({?MODULE, mcp_component_type}, mcp_server);
        <<"mcp-client">> ->
            ok;
        undefined ->
            ok
    end.

on_message_publish(
    #message{
        from = _ServerId,
        topic = <<"$mcp-server/presence/", _ServerIdAndName/binary>>,
        payload = <<>>
    } = Message
) ->
    %% Ignore MCP Server disconnected notifications
    {ok, Message};
on_message_publish(
    #message{
        from = ServerId,
        topic = <<"$mcp-server/presence/", ServerIdAndName/binary>>,
        payload = PresenceMsg
    } = Message
) ->
    {ServerId, ServerName} = split_id_and_server_name(ServerIdAndName),
    case mcp_bridge_message:decode_rpc_msg(PresenceMsg) of
        {ok, #{method := <<"notifications/server/online">>, params := Params}} ->
            case load_tools_from_register_msg(ServerId, ServerName, Params) of
                ok -> ok;
                {error, no_tools} -> erlang:put({?MODULE, need_list_tools}, true)
            end,
            initialize_mcp_server(#{server_id => ServerId, server_name => ServerName});
        {ok, Msg} ->
            ?SLOG(error, #{msg => unsupported_client_presence_msg, rpc_msg => Msg});
        {error, Reason} ->
            ?SLOG(error, #{msg => decode_rpc_msg_failed, reason => Reason})
    end,
    {ok, Message};
on_message_publish(
    #message{
        from = ServerId,
        topic = <<"$mcp-rpc/" ?MCP_CLIENTID_S "/", ServerIdAndName/binary>>,
        payload = PresenceMsg
    } = Message
) ->
    {ServerId, ServerName} = split_id_and_server_name(ServerIdAndName),
    case mcp_bridge_message:decode_rpc_msg(PresenceMsg) of
        {ok, #{id := ?INIT_REQ_ID, type := json_rpc_response}} ->
            send_initialized_notification(ServerId, ServerName),
            maybe_list_tools(ServerId, ServerName),
            ?SLOG(info, #{
                msg => received_initialize_response,
                server_id => ServerId,
                server_name => ServerName
            });
        {ok, #{id := ?INIT_REQ_ID} = Response} ->
            ?SLOG(error, #{msg => initialize_failed, rpc_response => Response});
        {ok, #{id := ?LIST_TOOLS_REQ_ID, type := json_rpc_response, result := Result}} ->
            ?SLOG(info, #{
                msg => received_list_tools_response,
                server_id => ServerId,
                server_name => ServerName
            }),
            load_tools_from_result(ServerId, ServerName, Result);
        {ok, #{id := ?LIST_TOOLS_REQ_ID} = Response} ->
            ?SLOG(error, #{msg => list_tools_failed, rpc_response => Response});
        {ok, #{id := Id, type := json_rpc_response, result := Result}} ->
            ?SLOG(info, #{
                msg => received_response, server_id => ServerId, server_name => ServerName
            }),
            handle_mcp_response(Id, Result);
        {ok, Msg} ->
            ?SLOG(warning, #{msg => unsupported_rpc_message_received, rpc_msg => Msg});
        {error, Reason} ->
            ?SLOG(error, #{msg => decode_rpc_msg_failed, reason => Reason})
    end,
    {ok, Message};
on_message_publish(Message) ->
    {ok, Message}.

on_message_delivered(
    _ClientInfo, #message{headers = #{?MCP_MSG_HEADER := McpRequest}, id = Id} = Message
) ->
    MqttId = emqx_guid:to_hexstr(Id),
    ok = maybe_cache_request(McpRequest, MqttId),
    {ok, mcp_bridge_message:complete_mqtt_msg(Message, MqttId)};
on_message_delivered(_ClientInfo, Message) ->
    {ok, Message}.

on_message_puback(_PacketId, #message{} = Message, PubRes, RC) ->
    NewRC =
        case RC of
            %% Demo: some service do not want to expose the error code (129) to client;
            %% so here it remap 129 to 128
            129 -> 128;
            _ -> RC
        end,
    ?SLOG(debug, #{
        msg => "mcp_bridge_on_message_puback",
        message => emqx_message:to_map(Message),
        pubres => PubRes,
        rc => NewRC
    }),
    {ok, NewRC}.

initialize_mcp_server(ServerInfo) ->
    InitReq = mcp_bridge_message:initialize_request(?INIT_REQ_ID, ?MCP_BRIDGE_INFO, #{}),
    Topic = mcp_bridge_message:get_topic(server_control, ServerInfo),
    InitReqMsg = mcp_bridge_message:make_mqtt_msg(Topic, InitReq, ?MCP_CLIENTID_B, #{}, 1),
    self() ! {deliver, Topic, InitReqMsg},
    ok.

send_initialized_notification(ServerId, ServerName) ->
    Notif = mcp_bridge_message:initialized_notification(),
    Topic = mcp_bridge_message:get_topic(rpc, #{
        mcp_clientid => ?MCP_CLIENTID_B,
        server_id => ServerId,
        server_name => ServerName
    }),
    NotifMsg = mcp_bridge_message:make_mqtt_msg(Topic, Notif, ?MCP_CLIENTID_B, #{}, 1),
    self() ! {deliver, Topic, NotifMsg},
    ok.

maybe_list_tools(ServerId, ServerName) ->
    case erlang:get({?MODULE, need_list_tools}) of
        true ->
            ListToolsReq = mcp_bridge_message:list_tools_request(?LIST_TOOLS_REQ_ID),
            Topic = mcp_bridge_message:get_topic(rpc, #{
                mcp_clientid => ?MCP_CLIENTID_B,
                server_id => ServerId,
                server_name => ServerName
            }),
            ListToolsReqMsg = mcp_bridge_message:make_mqtt_msg(
                Topic, ListToolsReq, ?MCP_CLIENTID_B, #{}, 1
            ),
            self() ! {deliver, Topic, ListToolsReqMsg},
            erlang:erase({?MODULE, need_list_tools}),
            ok;
        undefined ->
            ok
    end.

maybe_cache_request(#{wait_response := false} = Request, _) ->
    mcp_bridge_message:reply_caller(Request, delivered),
    ok;
maybe_cache_request(Request, MqttId) ->
    CacheK = {?MODULE, request_cache},
    Cache =
        case erlang:get(CacheK) of
            undefined -> #{};
            C -> C
        end,
    NewCache = maps:put(MqttId, Request, Cache),
    erlang:put(CacheK, NewCache),
    ok.

remove_expired_request(Cache) ->
    Now = erlang:system_time(millisecond),
    %% 30 seconds
    ExpiredThreshold = 30_000,
    maps:filter(
        fun(_MqttId, #{timestamp := Timestamp}) ->
            Now - Timestamp =< ExpiredThreshold
        end,
        Cache
    ).

handle_mcp_response(MqttId, Result) ->
    CacheK = {?MODULE, request_cache},
    Cache =
        case erlang:get(CacheK) of
            undefined -> #{};
            C -> C
        end,
    case maps:find(MqttId, Cache) of
        {ok, Request} ->
            mcp_bridge_message:reply_caller(Request, Result),
            NewCache = maps:remove(MqttId, Cache),
            erlang:put(CacheK, remove_expired_request(NewCache)),
            ok;
        error ->
            ?SLOG(warning, #{msg => unknown_mcp_response_id, mqtt_id => MqttId})
    end.

load_tools_from_result(_ServerId, ServerName, #{<<"tools">> := ToolsList}) ->
    mcp_bridge_tools:save_tools(ServerName, ToolsList).

load_tools_from_register_msg(_ServerId, ServerName, #{<<"meta">> := #{<<"tools">> := Tools}}) ->
    mcp_bridge_tools:save_tools(ServerName, Tools);
load_tools_from_register_msg(_ServerId, _ServerName, _Params) ->
    {error, no_tools}.

%%--------------------------------------------------------------------
%% Plugin callbacks
%%--------------------------------------------------------------------

%% @doc
%% - Return `{error, Error}' if the health check fails.
%% - Return `ok' if the health check passes.
%%
%% NOTE
%% For demonstration, we consider any port number other than 3306 unavailable.
on_health_check(_Options) ->
    ok.

%% @doc
%% - Return `{error, Error}' if the new config is invalid.
%% - Return `ok' if the config is valid and can be accepted.
on_config_changed(OldConfig, NewConfig) ->
    persistent_term:put(?MODULE, parse_config(NewConfig)),
    gen_server:cast(?MODULE, {on_changed, OldConfig, NewConfig}).

%%--------------------------------------------------------------------
%% Listeners
%%--------------------------------------------------------------------

restart_listener() ->
    _ = stop_listener(),
    start_listener().

start_listener() ->
    #{listening_address := ListeningAddress, certfile := Certfile, keyfile := Keyfile} = mcp_bridge:get_config(),
    #{scheme := Scheme, path := Path, authority := #{port := Port, host := Host}} =
        ListeningAddress,
    Paths =
        case Path of
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
            cowboy:start_clear(
                mcp_bridge_http_listener,
                [{port, Port}, {ip, Host}],
                #{env => #{dispatch => Dispatch}, middlewares => Middlewares}
            );
        <<"https">> ->
            SSLOptions = [
                {certfile, Certfile},
                {keyfile, Keyfile}
            ],
            cowboy:start_tls(
                mcp_bridge_http_listener,
                [{port, Port}, {ip, Host}] ++ SSLOptions,
                #{env => #{dispatch => Dispatch}, middlewares => Middlewares}
            );
        _ ->
            {error, {invalid_scheme, Scheme}}
    end.

stop_listener() ->
    cowboy:stop_listener(mcp_bridge_http_listener).

%%--------------------------------------------------------------------
%% Working with config
%%--------------------------------------------------------------------

%% @doc
%% Efficiently get the current config.
get_config() ->
    persistent_term:get(?MODULE, #{}).

%% gen_server callbacks

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

init([]) ->
    erlang:process_flag(trap_exit, true),
    PluginNameVsn = <<?PLUGIN_NAME, "-", ?PLUGIN_VSN>>,
    Config = emqx_plugin_helper:get_config(PluginNameVsn),
    ?SLOG(debug, #{
        msg => "mcp_bridge_init",
        config => Config
    }),
    persistent_term:put(?MODULE, parse_config(Config)),
    {ok, #{}}.

handle_call(_Request, _From, State) ->
    {reply, ok, State}.

handle_cast({on_changed, OldConfig, NewConfig}, State) ->
    case need_restart_listener(OldConfig, NewConfig) of
        true ->
            _ = restart_listener(),
            ?SLOG(warning, #{
                msg => mcp_bridge_listener_restarted_due_to_config_change, tag => ?MODULE
            });
        false ->
            ok
    end,
    {noreply, State};
handle_cast(_Request, State) ->
    {noreply, State}.

handle_info(_Request, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    persistent_term:erase(?MODULE),
    ok.

need_restart_listener(OldConfig, NewConfig) when is_map(OldConfig), is_map(NewConfig) ->
    %% find out the different keys that may affect the listener
    KeysToCheck = [<<"listening_address">>, <<"certfile">>, <<"keyfile">>],
    lists:any(
        fun(Key) ->
            maps:get(Key, OldConfig, undefined) =/= maps:get(Key, NewConfig, undefined)
        end,
        KeysToCheck
    ).

parse_config(#{<<"listening_address">> := URI} = Config) ->
    ListeningAddress = #{authority := #{host := Host} = Authority} = emqx_utils_uri:parse(URI),
    #{
        get_target_clientid_from => maps:get(
            <<"get_target_clientid_from">>, Config, <<"http_headers">>
        ),
        listening_address => ListeningAddress#{
            authority := Authority#{host := parse_address(Host)}
        },
        jwt_secret => maps:get(<<"jwt_secret">>, Config, <<"">>),
        certfile => maps:get(<<"certfile">>, Config, <<"">>),
        keyfile => maps:get(<<"keyfile">>, Config, <<"">>)
    }.

parse_address(Host) when is_binary(Host) ->
    Host1 = binary_to_list(Host),
    case inet_parse:address(Host1) of
        {ok, IP} ->
            IP;
        {error, _} ->
            {ok, IP} = inet:getaddr(Host1, inet),
            IP
    end.

split_id_and_server_name(Str) ->
    %% Split the server_id and server_name from the topic
    case string:split(Str, <<"/">>) of
        [ServerId, ServerName] -> {ServerId, ServerName};
        _ -> throw({error, {invalid_id_and_server_name, Str}})
    end.
