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

-record(state, {
            users  = none,
            status = none
         }).

%%====================================================================
%% gen_server functions
%%====================================================================

start_link() ->
  gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

init([]) ->
  Sessions = ets:new(mixtape_users,  [named_table]),
  Statuses = ets:new(mixtape_status, [named_table]),
  InitialState = #state{ users = Sessions, status = Statuses },
  {ok, InitialState}.

handle_info(_Info, State) ->
  {noreply, State}.

terminate(_Reason, _State) ->
  ok.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

%%====================================================================
%% Handler functions
%%====================================================================

handle_cast({register, Session}, State) ->
  register_user(Session, State),
  {noreply, State};
handle_cast({update_status, Status}, State) ->
  update_status(Status, State),
  {noreply, State}.

handle_call({status, PlaylistId}, _From, State) ->
  Status = find_status(PlaylistId, State),
  {reply, Status, State};

handle_call(user_count, _From, State) ->
  {reply, count_users(State), State};

handle_call(session_count, _From, #state{ users = Db }=State) ->
  {reply, ets:info(Db, size), State};

handle_call(dump_sessions, _From, #state{ users = Db }=State) ->
  {reply, ets:tab2list(Db), State};
handle_call(dump_statuses, _From, #state{ status = Db }=State) ->
  {reply, ets:tab2list(Db), State}.

%%====================================================================
%% Internal functions
%%====================================================================

register_user({PlaylistId, _UserId, _SocketPid}=Session, #state{ users = Users }) ->
  handle_playlist_lookup(ets:lookup(Users, PlaylistId), Session, Users).
handle_playlist_lookup([], {PlaylistId, UserId, SocketPid}, UsersDb) ->
  ets:insert(UsersDb, {PlaylistId, [{UserId, SocketPid}]});
handle_playlist_lookup([{PlaylistId, Users}], {_, UserId, SocketPid}, UsersDb) ->
  ets:insert(UsersDb, {PlaylistId, [{UserId, SocketPid} | Users]}).

update_status({PlaylistId, UserId, Offset, Position}, #state{ status = Status }) ->
  ets:insert(Status, {PlaylistId, {Offset, Position, UserId}}).

find_status(PlaylistId, #state{ status = Statuses }) ->
  handle_status_lookup(ets:lookup(Statuses, PlaylistId)).
handle_status_lookup([]) -> none;
handle_status_lookup([R|_]) -> R.

count_users(#state{ users = Db }) ->
  Sessions = ets:tab2list(Db),
  Users = lists:flatten(lists:map( fun({_, Users}) -> Users end, Sessions )),
  length(Users).
