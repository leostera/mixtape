%%%-------------------------------------------------------------------
%% @doc mixtape top level supervisor.
%% @end
%%%-------------------------------------------------------------------

-module(mixtape_session).

%% API
-export([
         find_session/1,
         list_sessions/0,
         register_client/3,
         session_count/0,
         sync/2
        ]).

%%====================================================================
%% API functions
%%====================================================================

register_client(WebSocketPid, PlaylistId, UserId) when is_pid(WebSocketPid) ->
  Msg = {register, {PlaylistId, UserId, WebSocketPid}},
  gen_server:cast(mixtape_session_store, Msg).

session_count() ->
  gen_server:call(mixtape_session_store, session_count).

list_sessions() ->
  gen_server:call(mixtape_session_store, dump_sessions).

find_session(PlaylistId) ->
  gen_server:call(mixtape_session_store, {find_session, PlaylistId}).

sync({PlaylistId, _UserId}, Cmd) ->
  Playlists = find_session(PlaylistId),
  ParsedCmd = parse_cmd(Cmd),
  [ sync_playlist(P, ParsedCmd) || P <- Playlists ].

sync_playlist({_PlaylistId, Users}, Cmd) -> [ sync_user(U, Cmd) || U <- Users ].

sync_user({UserId, SocketPid}, Cmd) when is_pid(SocketPid) ->
  io:format("Syncing User: ~p @ ~p with command ~p", [UserId, SocketPid, Cmd]),
  SocketPid ! Cmd.

parse_cmd(Cmd) -> #{ action => Cmd }.
