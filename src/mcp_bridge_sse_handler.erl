-module(mcp_bridge_sse_handler).

-include("mcp_bridge.hrl").
-include_lib("emqx_plugin_helper/include/logger.hrl").

-behaviour(cowboy_handler).

-export([
    init/2,
    info/3
]).

-export([path_specs/0]).

path_specs() ->
    [
        {"/sse", mcp_bridge_sse_handler, #{}},
        {"/sse/:session_id", mcp_bridge_sse_handler, #{}}
    ].

init(Req, State) ->
    Method = cowboy_req:method(Req),
    Path = cowboy_req:path(Req),
    handle_method(Method, Path, Req, State).

handle_method(<<"GET">>, <<"/sse">>, Req0, State) ->
    SessionId = binary:encode_hex(crypto:strong_rand_bytes(16)),
    %% When a client connects, the server MUST send an endpoint event containing
    %% a URI for the client to use for sending messages. All subsequent client
    %% messages MUST be sent as HTTP POST requests to this endpoint.
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
    ?SLOG(debug, #{msg => sse_post, tag => ?MODULE, session_id => SessionId, body => Body}),
    %% Quick reply with 202 Accepted, and dispatch the message to the session handler
    case mcp_bridge_session:dispatch_message(SessionId, Body) of
        ok ->
            Req2 = cowboy_req:reply(202, #{}, <<"Accepted">>, Req1),
            {ok, Req2, State};
        {error, session_not_found} ->
            ?SLOG(warning, #{msg => session_not_found, tag => ?MODULE, session_id => SessionId}),
            Req2 = cowboy_req:reply(404, #{}, <<"Session Not Found">>, Req1),
            {ok, Req2, State}
    end;
handle_method(UnsupportedMethod, _Path, Req0, State) ->
    ?SLOG(warning, #{msg => unsupported_http_method, tag => ?MODULE, method => UnsupportedMethod}),
    Req = cowboy_req:reply(405, #{}, <<"Method Not Allowed">>, Req0),
    {ok, Req, State}.

info({mcp_message, Message}, Req, State) ->
    case mcp_bridge_http_helper:handle_message(Message, Req, State) of
        {unsupported, NState} ->
            ?SLOG(warning, #{msg => ignoring_rpc_message, tag => ?MODULE, rpc_message => Message}),
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
