-module(mcp_bridge_http_handler).

-behaviour(cowboy_handler).

-export([ init/2
        ]).

init(Req, State) ->
    {ok, Req, State}.
