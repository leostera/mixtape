%%%-------------------------------------------------------------------
%% @doc mixtape public api
%% @end
%%%-------------------------------------------------------------------

-module(mixtape).

%% API
-export([
         find_session/1,
         list_sessions/0,
         list_statuses/0,
         register_client/3,
         session_count/0,
         status_for_playlist/1,
         sync/2,
         update_status/4,
         user_count/0
        ]).

%%====================================================================
%% API functions
%%====================================================================

register_client(WebSocketPid, PlaylistId, UserId) when is_pid(WebSocketPid) ->
  RegisterMsg = {register, {PlaylistId, UserId, WebSocketPid}},
  gen_server:cast(mixtape_session_store, RegisterMsg).

status_for_playlist(PlaylistId) ->
  gen_server:call(mixtape_session_store, {status, PlaylistId}).

update_status(PlaylistId, UserId, Offset, Position) ->
  UpdateMsg = {update_status, {PlaylistId, UserId, Offset, Position}},
  gen_server:cast(mixtape_session_store, UpdateMsg).

session_count() -> gen_server:call(mixtape_session_store, session_count).
user_count()    -> gen_server:call(mixtape_session_store, user_count).

list_sessions() -> gen_server:call(mixtape_session_store, dump_sessions).
list_statuses() -> gen_server:call(mixtape_session_store, dump_statuses).

find_session(PlaylistId) ->
  gen_server:call(mixtape_session_store, {find_session, PlaylistId}).

sync({PlaylistId, _UserId}, Cmd) ->
  ParsedCmd = parse_cmd(Cmd),
  Playlists = find_session(PlaylistId),
  Users = lists:flatten(find_users(Playlists)),
  sync_users(Users, ParsedCmd).

find_users([{_PlaylistId, Users} | T]) -> [ Users | find_users(T) ];
find_users([]) -> [].

sync_users(Users, Cmd) ->
  lists:foreach(fun (User) -> sync_user(User, Cmd) end, Users).

sync_user({UserId, SocketPid}, Cmd) when is_pid(SocketPid) ->
  io:format("Syncing User: ~p @ ~p with command ~p\n", [UserId, SocketPid, Cmd]),
  SocketPid ! Cmd.

parse_cmd(Cmd) -> #{ action => sync, status => Cmd }.
