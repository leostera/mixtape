%%%-------------------------------------------------------------------
%% @doc mixtape websocket handler
%% @end
%%%-------------------------------------------------------------------

-module(mixtape_ws_handler).

%% API
-export([
         init/2,
         websocket_handle/2,
         websocket_info/2,
         websocket_init/1
        ]).

-record(state, {
          socket_pid = self(),
          user_id,
          playlist_id
         }).

-opaque state() :: #state{}.

%%====================================================================
%% API functions
%%====================================================================

init(Req, _State) ->
  {cowboy_websocket, Req, initial_state(), #{ idle_timeout => 300000 }}.

initial_state() -> #state{ socket_pid = self() }.

%%====================================================================
%% ~ callbacks
%%====================================================================

websocket_init(State) ->
  {ok, State}.

websocket_handle({text, Msg}, State) ->
  handle(map_command(parse_message(Msg)), State);
websocket_handle(_Data, State) ->
  {ok, State}.

websocket_info({timeout, _Ref, Msg}, State) ->
  io:format("Closing socket! ~p ~p \n", [Msg, State]),
  {stop, State};
websocket_info({sync, {PlaylistId, Status}}, State) ->
  reply(message({bootstrap, {PlaylistId, Status}}), State);
websocket_info(Info, State) ->
  io:format("Unhandled Info Message: ~p \n", [Info]),
  {ok, State}.

%%====================================================================
%% Internal functions
%%====================================================================

parse_message(Msg) -> jiffy:decode(Msg, [return_maps]).

reply(Msg, State) -> {reply, {text, jiffy:encode(Msg)}, State}.

map_command(<<"ping">>) -> ping;
map_command(#{
  <<"action">> := <<"playback_update">>,
  <<"current_status">> := CurrentStatus,
  <<"offset_ms">> := Offset,
  <<"playlist_id">> := PlaylistId,
  <<"position">> := Position,
  <<"user_id">> := UserId
 }=Msg) when is_map(Msg) ->
  Status = #{
    current_status => binary_to_atom(CurrentStatus, utf8),
    track_number => Position,
    offset_ms => Offset,
    user => { UserId, self() }
   },
  {playback_update, PlaylistId, Status};
map_command(#{
  <<"action">> := <<"join">>,
  <<"playlist_id">> := PlaylistId,
  <<"user_id">> := UserId
 }=Msg) when is_map(Msg) -> {join, PlaylistId, UserId};
map_command(_) -> echo.


handle({join, PlaylistId, UserId}, State) ->
  User = {UserId, self()},
  mixtape:join_session(PlaylistId, User),
  SessionStatus = mixtape:session_status(PlaylistId),
  reply(message({bootstrap, {PlaylistId, SessionStatus}}), State);

handle({playback_update, PlaylistId, NewStatus}, State) ->
  reply(handle_playback(PlaylistId, NewStatus), State);

handle(Echo, State) -> reply(message(Echo), State).


message(ping) -> pong;
message({bootstrap, {PlaylistId, Status}}) ->
  #{
    current_status := CurrentStatus,
    track_number := Position,
    offset_ms := OffsetMs,
    user := { UserId, _ }
  } = Status,
  {[
    {type, bootstrap},
    {context, {[
                {current_status, CurrentStatus},
                {offset_ms, OffsetMs},
                {playlist_id, PlaylistId},
                {position, Position},
                {user, UserId}
               ]}}
   ]};
message(_) -> { [ {ack, message_received} ] }.


handle_playback(PlaylistId, #{ current_status := paused }=Status) ->
  mixtape:update_status(PlaylistId, Status),
  message({bootstrap, {PlaylistId, Status}});

handle_playback(PlaylistId, #{ current_status := playing }=Status) ->
  CurrentStatus = mixtape:session_status(PlaylistId),
  case diff(CurrentStatus, Status) of
    close_enough ->
      mixtape:update_status(PlaylistId, Status),
      ack;
    diverged ->
      mixtape:update_status(PlaylistId, Status),
      mixtape:sync(PlaylistId),
      message({bootstrap, {PlaylistId, Status}})
  end.

diff(#{ track_number := T }, #{ track_number := T }) -> close_enough;
diff(_, _) -> diverged.
