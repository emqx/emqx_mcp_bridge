-module(mcp_bridge_http_handler).

-behaviour(cowboy_handler).

-include_lib("emqx_plugin_helper/include/logger.hrl").

-export([
    init/2,
    path_specs/1
]).

path_specs(Path) ->
    [{Path, mcp_bridge_http_handler, #{}}].

init(Req, State) ->
    Method = cowboy_req:method(Req),
    Path = cowboy_req:path(Req),
    handle_method(Method, Path, Req, State).

handle_method(<<"GET">>, _Path, Req0, State) ->
    %% Return HTTP 405 Method Not Allowed, indicating that the server does not offer an SSE stream at this endpoint.
    Req = cowboy_req:reply(405, #{}, <<"SSE not supported">>, Req0),
    {ok, Req, State};
handle_method(<<"POST">>, _Path, Req0, State) ->
    {ok, Body, Req} = cowboy_req:read_body(Req0),
    ?SLOG(debug, #{msg => received_http_post, tag => ?MODULE, body => Body}),
    case mcp_bridge_http_helper:handle_message(Body, Req, State) of
        {unsupported, NState} ->
            ?SLOG(warning, #{msg => unsupported_rpc_msg, tag => ?MODULE, rpc_message => Body}),
            Req1 = cowboy_req:reply(501, #{}, <<"Unsupported RPC message">>, Req),
            {ok, Req1, NState};
        {Response, NState} ->
            Req1 = cowboy_req:reply(
                200, #{<<"content-type">> => <<"application/json">>}, Response, Req
            ),
            {ok, Req1, NState}
    end;
handle_method(UnsupportedMethod, _Path, Req0, State) ->
    ?SLOG(warning, #{msg => unsupported_http_method, tag => ?MODULE, method => UnsupportedMethod}),
    Req = cowboy_req:reply(405, #{}, <<"Method Not Allowed">>, Req0),
    {ok, Req, State}.
