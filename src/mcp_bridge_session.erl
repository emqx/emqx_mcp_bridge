-module(mcp_bridge_session).

-include_lib("emqx_plugin_helper/include/logger.hrl").

-behaviour(gen_server).

-export([start_link/0, stop/0]).
-export([register_session/2, dispatch_message/2]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

start_link() ->
    ets:new(?MODULE, [named_table, public, set]),
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

stop() ->
    gen_server:stop(?MODULE).

register_session(SessionId, Pid) when is_binary(SessionId), is_pid(Pid) ->
    gen_server:call(?MODULE, {register, SessionId, Pid}).

dispatch_message(SessionId, Message) when is_binary(SessionId), is_binary(Message) ->
    case ets:lookup(?MODULE, SessionId) of
        [{SessionId, Pid}] ->
            Pid ! {mcp_message, Message},
            ok;
        [] ->
            {error, session_not_found}
    end.

init([]) ->
    {ok, #{}}.

handle_call({register, SessionId, Pid}, _From, State) ->
    _ = erlang:monitor(process, Pid),
    ets:insert(?MODULE, {SessionId, Pid}),
    ets:insert(?MODULE, {Pid, SessionId}),
    {reply, ok, State};
handle_call(_Request, _From, State) ->
    {reply, ok, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info({'DOWN', _Ref, process, Pid, _Reason}, State) ->
    %% Remove all sessions associated with the downed Pid
    case ets:lookup(?MODULE, Pid) of
        [{Pid, SessionId}] ->
            ets:delete(?MODULE, SessionId),
            ets:delete(?MODULE, Pid);
        [] ->
            ok
    end,
    {noreply, State};
handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
