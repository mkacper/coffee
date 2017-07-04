%%%-------------------------------------------------------------------
%% @doc coffee top level supervisor.
%% @end
%%%-------------------------------------------------------------------

-module(coffee_sup).

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
    ChildSpec = #{id => coffee_fsm,
                  start => {coffee_fsm, start_link, []},
                  shutdown => brutal_kill},
    {ok, { {one_for_all, 0, 1}, [ChildSpec]} }.

%%====================================================================
%% Internal functions
%%====================================================================
