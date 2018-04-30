%%%-------------------------------------------------------------------
%% @doc mixtape top level supervisor.
%% @end
%%%-------------------------------------------------------------------

-module(mixtape_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

%%====================================================================
%% API functions
%%====================================================================

start_link() ->
  supervisor:start_link({local, ?SERVER}, ?MODULE, []).

%%====================================================================
%% Supervisor callbacks
%%====================================================================

%% Child :: {Id,StartFunc,Restart,Shutdown,Type,Modules}
init([]) ->
  {ok, { supervision_flags(), child_specs() } }.

%%====================================================================
%% Internal functions
%%====================================================================

supervision_flags() -> #{
  strategy  => one_for_all,
  intensity => 0,
  period    => 1
 }.

child_specs() -> [ #{
  id       => mixtape_session_store,
  restart  => permanent,
  shutdown => brutal_kill,
  start    => { mixtape_session_store, start_link, [] },
  type     => supervisor
 } ].
