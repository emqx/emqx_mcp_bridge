-module(mcp_bridge_http_helper).

-include("mcp_bridge.hrl").
-include_lib("emqx_plugin_helper/include/logger.hrl").

-export([
    handle_message/3
]).

handle_message(Message, Req, State) ->
    case mcp_bridge_message:decode_rpc_msg(Message) of
        {ok, #{type := json_rpc_request, method := <<"tools/call">>} = RpcMsg} ->
            Headers = cowboy_req:headers(Req),
            JwtClaims = maps:get(jwt_claims, Req, #{}),
            Response = mcp_bridge_message:send_tools_call(Headers, JwtClaims, RpcMsg, true, 3_000),
            {Response, State};
        {ok, #{type := json_rpc_request, method := <<"initialize">>, id := Id}} ->
            Response = mcp_bridge_message:initialize_response(Id, ?MCP_BRIDGE_INFO, #{}),
            {Response, State};
        {ok, #{type := _}} ->
            {unsupported, State};
        {error, Reason} ->
            ?SLOG(error, #{
                msg => invalid_rpc_message, tag => ?MODULE, reason => Reason, message => Message
            }),
            {mcp_bridge_message:json_rpc_error(-1, -32600, <<"Invalid JSON">>, #{}), State}
    end.
