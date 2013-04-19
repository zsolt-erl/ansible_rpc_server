-module(rrpc_services_sup).

-behaviour(supervisor).

-include("rrpc.hrl").

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

%% Helper macro for declaring children of supervisor
-define(CHILD(I, Type), {I, {I, start_link, []}, permanent, 5000, Type, [I]}).

%% ===================================================================
%% API functions
%% ===================================================================

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

init([]) ->
    ?log("starting"),
%%     TestService =  ?CHILD(rrpc_test_service, worker), 

    {ok, { {one_for_one, 5, 10}, []} }.

