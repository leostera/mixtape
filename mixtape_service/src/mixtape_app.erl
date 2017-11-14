%%%-------------------------------------------------------------------
%% @doc mixtape public API
%% @end
%%%-------------------------------------------------------------------

-module(mixtape_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

%%====================================================================
%% API
%%====================================================================

start(_StartType, _StartArgs) ->
  start_cowboy_server(),
  mixtape_sup:start_link().

%%--------------------------------------------------------------------
stop(_State) ->
  ok.

%%====================================================================
%% Internal functions
%%====================================================================

server_options() -> [{port, 2112}].
server_config(Dispatch) -> #{ env => #{ dispatch => Dispatch } }.

router_options() -> [
                     { '_', [
                             {"/", mixtape_ws_handler, []}
                            ]}
                    ].

start_cowboy_server() ->
  Dispatch = cowboy_router:compile(router_options()),
  {ok, _} = cowboy:start_clear(http, server_options(), server_config(Dispatch)).
