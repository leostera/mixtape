%%%-------------------------------------------------------------------
%% @doc mixtape public api
%% @end
%%%-------------------------------------------------------------------

-module(mixtape).

%% API
-export([
         blank_status/1,
         find_session/1,
         join_session/2,
         list_sessions/0,
         quit_session/2,
         session_count/0,
         session_status/1,
         sync/1,
         update_status/2,
         user_count/0
        ]).


-type playback_status() :: playing | paused.

-type user_id() :: <<>>.
-type playlist_id() :: <<>>.

-type user() :: { user_id(), pid() }.

-type status() :: #{
        current_status => playback_status(),
        track_number => integer(),
        offset_ms => integer(),
        user => user()
       }.

-type session() :: { playlist_id(), status(), [user()] }.

-export_type([
              playback_status/0,
              playlist_id/0,
              session/0,
              status/0,
              user/0,
              user_id/0
             ]).

%%====================================================================
%% API functions
%%====================================================================

sync(PlaylistId) ->
  {PlaylistId, Status, Users} = find_session(PlaylistId),
  [ sync_user(U, {PlaylistId, Status}) || U <- Users].

sync_user({UserId, Pid}, {PlaylistId, Status}) ->
  Pid ! {sync, {PlaylistId, Status}}.

-spec join_session(playlist_id(), user()) -> ok.
join_session(PlaylistId, {_UserId, WebSocketPid}=User) when is_pid(WebSocketPid) ->
  JoinMsg = {join, {PlaylistId, User}},
  gen_server:cast(mixtape_session_store, JoinMsg).

-spec quit_session(playlist_id(), user()) -> ok.
quit_session(PlaylistId, {_UserId, WebSocketPid}=User) when is_pid(WebSocketPid) ->
  QuitMsg = {quit, {PlaylistId, User}},
  gen_server:cast(mixtape_session_store, QuitMsg).

-spec session_status(playlist_id()) -> none | status().
session_status(PlaylistId) ->
  case find_session(PlaylistId) of
    none -> none;
    {_PlaylistId, Status, _Users} -> Status
  end.

-spec update_status(playlist_id(), status()) -> status().
update_status(PlaylistId, Status) ->
  UpdateMsg = {update_status, {PlaylistId, Status}},
  [{PlaylistId, UpdatedStatus, _}] = gen_server:call(mixtape_session_store, UpdateMsg),
  UpdatedStatus.

-spec session_count() -> integer().
session_count() -> gen_server:call(mixtape_session_store, session_count).

-spec user_count() -> integer().
user_count() -> gen_server:call(mixtape_session_store, user_count).

-spec list_sessions() -> [session()].
list_sessions() -> gen_server:call(mixtape_session_store, dump).

-spec find_session(playlist_id()) -> none | session().
find_session(PlaylistId) ->
  case gen_server:call(mixtape_session_store, {find_session, PlaylistId}) of
    [] -> none;
    [Session|_] -> Session
  end.

-spec blank_status(user()) -> status().
blank_status(User) -> #{
               current_status => playing,
               track_number => 1,
               offset_ms => 0,
               user => User
              }.
