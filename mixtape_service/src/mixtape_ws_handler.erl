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

%%====================================================================
%% API functions
%%====================================================================

init(Req, Opts) ->
  {cowboy_websocket, Req, Opts}.

%%====================================================================
%% ~ callbacks
%%====================================================================

websocket_init(State) ->
  {ok, State}.

websocket_handle({text, Msg}, State) ->
  ParsedMessage = map_to_tuple(parse_message(Msg)),
  io:format("Parsed message: ~p", [ParsedMessage]),
  handle(ParsedMessage, State);
websocket_handle(_Data, State) ->
  {ok, State}.

websocket_info({timeout, _Ref, Msg}, State) ->
  erlang:start_timer(500, self(), <<"keep-alive">>),
  reply(Msg, State);
websocket_info(#{ action := Action }=Msg, State) ->
  reply(Msg, State);
websocket_info(Info, State) ->
  io:format("~p", [Info]),
  {ok, State}.

%%====================================================================
%% Internal functions
%%====================================================================

parse_message(Msg) -> jiffy:decode(Msg, [return_maps]).

reply(Msg, State) -> {reply, {text, jiffy:encode(Msg)}, State}.

map_to_tuple(#{
  <<"action">> := Action,
  <<"playlist_id">> := PlaylistId,
  <<"user_id">> := UserId
 }=Msg) when is_map(Msg) -> {binary_to_existing_atom(Action, utf8), PlaylistId, UserId}.

handle({register, PlaylistId, UserId}, State) ->
  mixtape_session:register_client(self(), PlaylistId, UserId),
  reply(message(connected), State);
handle({pause, PlaylistId, UserId}, State) ->
  mixtape_session:sync({PlaylistId, UserId}, pause),
  reply(message(pause), State);
handle(_, State) -> {ok, State}.

message(connected) -> { [ {connection_status, connected} ] };
message(_) -> { [ {ack, message_received} ] }.
