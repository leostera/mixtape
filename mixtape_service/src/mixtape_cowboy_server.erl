%%%-------------------------------------------------------------------
%% @doc mixtape cowboy server
%% @end
%%%-------------------------------------------------------------------

-module(mixtape_cowboy_server).

-export([start/0]).

%%====================================================================
%% API
%%====================================================================

start() ->
  Dispatch = cowboy_router:compile(router_options()),
  {ok, _} = cowboy:start_clear(http, server_options(), server_config(Dispatch)).

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
