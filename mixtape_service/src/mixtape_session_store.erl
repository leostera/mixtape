%%%-------------------------------------------------------------------
%% @doc mixtape session store
%% @end
%%%-------------------------------------------------------------------

-module(mixtape_session_store).

-behavior(gen_server).

%% API
-export([start_link/0]).

%% gen_server callbacks
-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

%%====================================================================
%% API functions
%%====================================================================

start_link() ->
  gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

init([]) ->
  Db = ets:new(mixtape_sessions, [named_table]),
  {ok, Db}.

handle_info(_Info, State) ->
  {noreply, State}.

terminate(_Reason, _State) ->
  ok.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

%%====================================================================
%% Handler functions
%%====================================================================

handle_cast({register, {PlaylistId, UserId, SocketPid}}, Db) ->
  Users = ets:lookup(Db, PlaylistId),
  ets:insert(Db, {PlaylistId, [{UserId, SocketPid} | Users]}),
  {noreply, Db}.

handle_call(session_count, Db) ->
  {reply, ets:info(Db, size), Db}.
