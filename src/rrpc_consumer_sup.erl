-module(rrpc_consumer_sup).

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
    ConsumerSpec =  ?CHILD(rrpc_consumer, worker), 

    {ok, { {simple_one_for_one, 5, 10}, [ ConsumerSpec ]} }.

