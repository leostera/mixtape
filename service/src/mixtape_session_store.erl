%%%-------------------------------------------------------------------
%% @doc mixtape session store
%% @end
%%%-------------------------------------------------------------------

-module(mixtape_session_store).

-behavior(gen_server).

%% API
-export([start_link/0]).

%% gen_server callbacks
-export([
         code_change/3,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         init/1,
         terminate/2
        ]).

-record(state, { sessions = none }).
-opaque state() :: #state{}.

-type cast_message() :: { join, {mixtape:playlist_id(), mixtape:user()} }
                      | { quit, {mixtape:playlist_id(), mixtape:user()} }
                      | { update_status, {mixtape:playlist_id(), mixtape:status()} }.

-type call_message() :: dump
                      | session_count
                      | user_count
                      | { find_session, mixtape:playlist_id() }
                      | { update_status, {mixtape:playlist_id(), mixtape:status()} }.

-export_type([
              call_message/0,
              cast_message/0,
              state/0
             ]).

%%====================================================================
%% gen_server functions
%%====================================================================

start_link() ->
  gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

init([]) ->
  Sessions = ets:new(mixtape_sessions,  [named_table]),
  InitialState = #state{ sessions = Sessions },
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

-spec handle_cast(cast_message(), state()) -> {noreply, state()}.
handle_cast({join, {PlaylistId, User}}, State) ->
  join(PlaylistId, User, State),
  {noreply, State};

handle_cast({quit, {PlaylistId, User}}, State) ->
  quit(PlaylistId, User, State),
  {noreply, State}.

-spec handle_call(call_message(), {pid(), _}, state()) -> {reply, _, state()}.
handle_call({update_status, {PlaylistId, Status}}, _From, State) ->
  update_status(PlaylistId, Status, State),
  {reply, find_session(PlaylistId, State), State};

handle_call({find_session, PlaylistId}, _From, State) ->
  {reply, find_session(PlaylistId, State), State};

handle_call(user_count, _From, State) ->
  {reply, count_users(State), State};

handle_call(session_count, _From, #state{ sessions = Db }=State) ->
  {reply, ets:info(Db, size), State};

handle_call(dump, _From, #state{ sessions = Db }=State) ->
  {reply, ets:tab2list(Db), State}.

%%====================================================================
%% Internal functions
%%====================================================================

-spec join(mixtape:playlist_id(), mixtape:user(), state()) -> true.
join(PlaylistId, User, #state{ sessions = Db }=State) ->
  LookupResult = find_session(PlaylistId, State),
  UpdatedSession = do_join(LookupResult, PlaylistId, User),
  ets:insert(Db, UpdatedSession).

-spec quit(mixtape:playlist_id(), mixtape:user(), state()) -> true.
quit(PlaylistId, User, #state{ sessions = Db }=State) ->
  LookupResult = find_session(PlaylistId, State),
  UpdatedSession = do_quit(LookupResult, PlaylistId, User),
  ets:insert(Db, UpdatedSession).

-spec update_status(mixtape:playlist_id(), mixtape:status(), state()) -> true.
update_status(PlaylistId, NewStatus, #state{ sessions = Db }=State) ->
  case find_session(PlaylistId, State) of
    [] ->
      #{ user := User } = NewStatus,
      ets:insert(Db, {PlaylistId, NewStatus, [User]});
    [{PlaylistId, _CurrentStatus, Users}|_] ->
      ets:insert(Db, {PlaylistId, NewStatus, Users})
  end.

-spec find_session(mixtape:playlist_id(), state()) -> [mixtape:session()].
find_session(PlaylistId, #state{ sessions = Db }) -> ets:lookup(Db, PlaylistId).

-spec count_users(state()) -> integer().
count_users(#state{ sessions = Db }) ->
  Sessions = ets:tab2list(Db),
  Users = lists:flatten(lists:map(fun({_, _, Users}) -> Users end, Sessions)),
  length(Users).

-spec do_join([mixtape:session()], mixtape:playlist_id(), mixtape:user()) -> mixtape:session().
do_join([], PlaylistId, User) ->
  {PlaylistId, mixtape:blank_status(User), [User]};
do_join([{_, Status, Users}], PlaylistId, {UserId, _SocketPid}=User) ->
  NewUsers = lists:filter(fun({Uid, _Pid}) -> Uid =/= UserId end, Users),
  {PlaylistId, Status, [User | NewUsers]}.

-spec do_quit([mixtape:session()], mixtape:playlist_id(), mixtape:user()) -> mixtape:session().
do_quit([], PlaylistId, User) ->
  {PlaylistId, mixtape:blank_status(User), []};
do_quit([{PlaylistId, Status, Users}], PlaylistId, {UserId, _SocketPid}) ->
  NewUsers = lists:filter(fun({Uid, _Pid}) -> Uid =/= UserId end, Users),
  {PlaylistId, Status, NewUsers}.
