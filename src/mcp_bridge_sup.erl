-module(mcp_bridge_sup).

-behaviour(supervisor).

-export([start_link/0]).

-export([init/1]).

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
    ConfigChildSpec = #{
        id => mcp_bridge,
        start => {mcp_bridge, start_link, []},
        restart => permanent,
        shutdown => 5000,
        type => worker,
        modules => [mcp_bridge]
    },
    SupFlags = #{
        strategy => one_for_all,
        intensity => 100,
        period => 10
    },
    {ok, {SupFlags, [ConfigChildSpec]}}.
