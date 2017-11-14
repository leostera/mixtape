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
  {reply, {text, <<"That's what she said! ", Msg/binary>>}, State};
websocket_handle(_Data, State) ->
  {ok, State}.

websocket_info({timeout, _Ref, Msg}, State) ->
  erlang:start_timer(1000, self(), <<"How' you doin'?">>),
  {reply, {text, Msg}, State};
websocket_info(_Info, State) ->
  {ok, State}.

%%====================================================================
%% Internal functions
%%====================================================================

% handle({register, PlaylistId, UserId}) ->
  % SessionId = mixtape_sessions:register(self(), playlist_id(Msg), user_id(Msg))),
