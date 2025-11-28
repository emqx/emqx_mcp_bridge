-module(mcp_bridge_http_handler).

-behaviour(cowboy_handler).

-export([ init/2
        , path_specs/1
        ]).

path_specs(Path) ->
    [{Path, mcp_bridge_http_handler, #{}}].

init(Req, State) ->
    {ok, Req, State}.
