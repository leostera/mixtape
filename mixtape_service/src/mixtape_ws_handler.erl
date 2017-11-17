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
websocket_info(#{ action := sync }=Msg, State) ->
  io:format("Syncing: ~p\n", [Msg]),
  reply(Msg, State);
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
  <<"offset_ms">> := Offset,
  <<"playlist_id">> := PlaylistId,
  <<"position">> := Position,
  <<"user_id">> := UserId
 }=Msg) when is_map(Msg) -> {playback_update, PlaylistId, UserId, Offset, Position};
map_command(#{
  <<"action">> := Action,
  <<"playlist_id">> := PlaylistId,
  <<"user_id">> := UserId
 }=Msg) when is_map(Msg) -> {binary_to_existing_atom(Action, utf8), PlaylistId, UserId}.

handle({register, PlaylistId, UserId}, State) ->
  mixtape:register_client(self(), PlaylistId, UserId),
  SessionStatus = mixtape:status_for_playlist(PlaylistId),
  reply(message(register, SessionStatus), State);

handle({playback_update, PlaylistId, UserId, Offset, Position}, State) ->
  mixtape:update_status(PlaylistId, UserId, Offset, Position),
  reply(message(playback_update), State);

handle(Echo, State) -> reply(message(Echo), State).

message(register, none) ->
  {[ {connection_status, connected}, {info, first_user} ]};
message(register, {PlaylistId, {Offset, Position, _UserId}}) ->
  {[
    {connection_status, connected},
    {info, bootstrap},
    {context, {[
                {playlist_id, PlaylistId},
                {offset_ms, Offset},
                {position, Position}
               ]}}
   ]}.
message(ping) -> pong;
message(_) -> { [ {ack, message_received} ] }.
