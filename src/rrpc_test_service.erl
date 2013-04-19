%% Example JSON-RPC service implementation
%%
%% handle_call/3 needs to handle jsonrpc messages of the format:
%%      Request = {jsonrpc, Method, RequestInfo, Args}, _From, State}
%%      Method      :: binary(),
%%      RequestInfo :: jsonobj(),
%%      Args        :: list()
%%
%% Reply :: jsonrpc_response()
%% jsonrpc_response() = {result | error, json()} | {result | error, json(), ResponseInfo :: jsonobj()}
%%
%% handle_call({jsonrpc, <<"increment">>, RequestInfo, [Step]}, _From, State = #state{counter = Counter}) ->
%%      NewCounter = Counter + Step,
%%      Reply = {result, {obj, [{"Counter", NewCounter}]}},
%%      {reply, Reply, State#state{counter = NewCounter}};
%%
%%
%% needs to export: rpc_service_description/0



-module(rrpc_test_service).

-behaviour(gen_server).

-include("lib/erlang-rfc4627/include/rfc4627_jsonrpc.hrl").
-include("rrpc.hrl").

%% API
-export([start_link/0]).

%% gen_server callbacks
-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

%% exports for RPC service registration
-export([rpc_service_description/0]).

-export([increment/1]).

-record(state, {counter = 0}).


%% http://tonyg.github.com/erlang-rfc4627/doc/
rpc_service_description()->
    IncrementProc = rrpc_util:create_procedure_description([
                {name,        "increment"},
                {summary,     "This is a simple procedure to increment a counter"},
                {help,        "No help here. You're on your own."},
                {get_allowed, false},
                {return,      undefined},
                {params, [
                        [{name, "step"}, {type, num}]
                        ]}
                ]),

    Service = rrpc_util:create_service_description([
                {name,    "test_service"},
                {id,      "some_id"},
                {version, "0.1"},
                {summary, "Simple test service"},
                {help,    "You can figure this out by yourself."},
                {procs, [
                        IncrementProc]}
                ]),
    Service.





%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%%
%% @spec start_link() -> {ok, Pid} | ignore | {error, Error}
%% @end
%%--------------------------------------------------------------------
start_link() ->
    ?log("starting"),
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).


increment(Step)->
    ?log("CALLED INCREMENT"),
    Reply = gen_server:call(?MODULE, {jsonrpc, <<"increment">>, [], [Step]}),
    ?log("SERVICE REPLY:~p", [Reply]),
    {ok, Reply}.

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Initializes the server
%%
%% @spec init(Args) -> {ok, State} |
%%                     {ok, State, Timeout} |
%%                     ignore |
%%                     {stop, Reason}
%% @end
%%--------------------------------------------------------------------
init([]) ->
    {ok, #state{}}.


%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling call messages
%%
%% @spec handle_call(Request, From, State) ->
%%                                   {reply, Reply, State} |
%%                                   {reply, Reply, State, Timeout} |
%%                                   {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, Reply, State} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_call({jsonrpc, <<"increment">>, RequestInfo, [Step]}, _From, State = #state{counter = Counter}) ->
    NewCounter = Counter + Step,
    Reply = {result, {obj, [{"Counter", NewCounter}]}},
    {reply, Reply, State#state{counter = NewCounter}};


handle_call(Request = {jsonrpc, Method, RequestInfo, Args}, _From, State) ->
    ?log("UNKNOWN JSONRPC REQUEST: ~p", [Request]),
    Reply = ok,
    {reply, Reply, State};

handle_call(Request, _From, State) ->
    io:format("UNKNOWN REQUEST: ~p", [Request]),
    Reply = ok,
    {reply, Reply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling cast messages
%%
%% @spec handle_cast(Msg, State) -> {noreply, State} |
%%                                  {noreply, State, Timeout} |
%%                                  {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_cast(_Msg, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling all non call/cast messages
%%
%% @spec handle_info(Info, State) -> {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------

handle_info(_Info, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_server terminates
%% with Reason. The return value is ignored.
%%
%% @spec terminate(Reason, State) -> void()
%% @end
%%--------------------------------------------------------------------
terminate(_Reason, _State) ->
    ok.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert process state when code is changed
%%
%% @spec code_change(OldVsn, State, Extra) -> {ok, NewState}
%% @end
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
