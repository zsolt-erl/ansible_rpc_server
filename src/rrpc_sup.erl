
-module(rrpc_sup).

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
    ServicesSup = ?CHILD(rrpc_services_sup, supervisor),
    ConsumerSup = ?CHILD(rrpc_consumer_sup, supervisor),
    Publisher = ?CHILD(rrpc_test_publisher, worker),
    Consumer = ?CHILD(rrpc_consumer, worker),
    AnsibleModuleRunner = ?CHILD(rrpc_amr, worker),
    AnsiblePlaybookRunner = ?CHILD(rrpc_apr, worker),

    {ok, { {one_for_one, 5, 10}, [ ServicesSup, ConsumerSup, Publisher, AnsibleModuleRunner, AnsiblePlaybookRunner ]} }.

