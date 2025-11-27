-module(mcp_bridge_sse_handler).

-include("mcp_bridge.hrl").
-include_lib("emqx_plugin_helper/include/logger.hrl").

-behaviour(cowboy_handler).

-export([ init/2
        , info/3
        ]).

-export([path_specs/0]).

path_specs() ->
    [ {"/sse", mcp_bridge_sse_handler, #{}}
    , {"/sse/:session_id", mcp_bridge_sse_handler, #{}}
    ].

init(Req, State) ->
    Method = cowboy_req:method(Req),
    Path = cowboy_req:path(Req),
    handle_method(Method, Path, Req, State).

handle_method(<<"GET">>, <<"/sse">>, Req0, State) ->
    SessionId = binary:encode_hex(crypto:strong_rand_bytes(16)),
    %% When a client connects, the server MUST send an endpoint event containing a URI for the client to use for sending messages. All subsequent client messages MUST be sent as HTTP POST requests to this endpoint.
    EndpointEvent = #{
        event => <<"endpoint">>,
        data => <<"/sse/", SessionId/binary>>
    },
    Req = cowboy_req:stream_reply(200, #{<<"content-type">> => <<"text/event-stream">>}, Req0),
    ok = cowboy_req:stream_events(EndpointEvent, nofin, Req),
    ok = mcp_bridge_session:register_session(SessionId, self()),
    {cowboy_loop, Req, State};

handle_method(<<"POST">>, <<"/sse/", SessionId/binary>>, Req, State) ->
    {ok, Body, Req1} = cowboy_req:read_body(Req),
    %% Process the message body as needed
    io:format("Received message for session ~p: ~p~n", [SessionId, Body]),
    %% Quick reply with 202 Accepted, and dispatch the message to the session handler
    case mcp_bridge_session:dispatch_message(SessionId, Body) of
        ok ->
            Req2 = cowboy_req:reply(202, #{}, <<"Accepted">>, Req1),
            {ok, Req2, State};
        {error, session_not_found} ->
            ?SLOG(warning, #{msg => session_not_found, tag => ?MODULE, session_id => SessionId}),
            Req2 = cowboy_req:reply(404, #{}, <<"Session Not Found">>, Req1),
            {ok, Req2, State}
    end.

info({mcp_message, Message}, Req, State) ->
    case handle_message(Message, Req, State) of
        {undefined, NState} ->
            {ok, Req, NState};
        {Response, NState} ->
            Event = #{
                event => <<"message">>,
                data => Response
            },
            ok = cowboy_req:stream_events(Event, nofin, Req),
            {ok, Req, NState}
    end;
info(_Info, Req, State) ->
    ?SLOG(warning, #{msg => received_unexpected_info, tag => ?MODULE, info => _Info}),
    {ok, Req, State}.

handle_message(Message, Req, State) ->
    case mcp_bridge_message:decode_rpc_msg(Message) of
        {ok, #{type := json_rpc_request, method := <<"tools/call">>} = RpcMsg} ->
            Headers = cowboy_req:headers(Req),
            JwtClaims = maps:get(jwt_claims, State, #{}),
            Response = mcp_bridge_message:send_tools_call(Headers, JwtClaims, RpcMsg, true, 3_000),
            {Response, State};
        {ok, #{type := json_rpc_request, method := <<"initialize">>, id := Id}} ->
            Response = mcp_bridge_message:initialize_response(Id, ?MCP_BRIDGE_INFO, #{}),
            {Response, State};
        {ok, #{type := _}} ->
            %% Ignore responses, notifications, and errors from MCP client
            {undefined, State};
        {error, Reason} ->
            ?SLOG(error, #{msg => invalid_rpc_message, tag => ?MODULE, reason => Reason, message => Message}),
            {<<"error: invalid rpc message">>, State}
    end.
