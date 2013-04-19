-module(rrpc_test_publisher).

-behaviour(gen_server).

-include("rrpc.hrl").
-include("amqp_client.hrl").
-include("lib/erlang-rfc4627/include/rfc4627.hrl").

-define(SERVICE, test_service).
-define(METHOD, increment).


%% API
-export([start_link/0, rpc_request/1, sample_request/0, publish/1]).

%% gen_server callbacks
-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

-record(state, {connection, channel, rpc_queue}).


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


rpc_request(Request) ->
    gen_server:call(?MODULE, {rpc_request, Request}).

publish(Binary) ->
    gen_server:call(?MODULE, {publish, Binary}).

sample_request() ->
    [{service, <<"test_service">>},
     {method,  <<"increment">>},
     {params,  [12]},
     {id,      999},
     {reply_queue, <<"whatever">>}].



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
    ?log("starting"),
    {ok, Connection} = amqp_connection:start(#amqp_params_network{username = ?conf(rmq_username), password = ?conf(rmq_password), host = ?conf(rmq_host) }),
    {ok, Channel}    = amqp_connection:open_channel(Connection),
    ?log("connected"),
    %% #'tx.select_ok'{} = amqp_channel:call(Channel, #'tx.select'{}),

%%     ExDeclare = #'exchange.declare'{exchange = <<"test">>},
%%     #'exchange.declare_ok'{} = amqp_channel:call(Channel, ExDeclare),
%%     ?log("exchange created"),

    {ok, #state{connection=Connection, channel=Channel  }}.


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

handle_call({rpc_request, Proplist}, _From, State = #state{channel=Channel}) ->
    ?log("Sending request: ~p", [Proplist]),


    ReplyQueue    = ?gv(reply_queue, Proplist),
    CorrelationId = ?gv(id, Proplist),

    Service       = ?gv(service, Proplist),
    Method        = ?gv(method, Proplist),
    Params        = ?gv(params, Proplist),

    Exchange      = <<"test">>,
    Routingkey    = <<"rpc_queue/",Service/binary>>,

    Payload = {obj, [
                {"id", CorrelationId},
                {"method", Method},
                {"params", Params}
                ]},
    BinEncodedPayload = list_to_binary( rfc4627:encode(Payload) ),

    Props = #'P_basic'{
    %%        correlation_id = <<CorrelationId:64>>,
            content_type = <<"application/octet-stream">>,
            reply_to = ReplyQueue
            },
    Publish = #'basic.publish'{exchange = Exchange,
                               routing_key = Routingkey,
                               mandatory = true},
    Result = amqp_channel:cast(Channel, Publish, #amqp_msg{props = Props, payload = BinEncodedPayload}),

    ?log(Result),

    Reply = ok,
    {reply, Reply, State};

handle_call({publish, Binary}, _From, State = #state{channel=Channel}) ->
    ?log("Publishing: ~p", [Binary]),

    Exchange      = <<"test">>,
    Routingkey    = <<"whatever">>,

%%     BinEncodedPayload = list_to_binary( rfc4627:encode(Payload) ),

    Props = #'P_basic'{content_type = <<"application/octet-stream">>},
    Publish = #'basic.publish'{exchange = Exchange,
                               routing_key = Routingkey,
                               mandatory = true},
    Result = amqp_channel:cast(Channel, Publish, #amqp_msg{props = Props, payload = Binary}),

    ?log(Result),

    Reply = ok,
    {reply, Reply, State};

handle_call(_Request, _From, State) ->
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
handle_info(timeout, State = #state{connection=Cn, channel=Ch}) ->
    Ex= <<"test">>,
    Payload = <<"piggy">>,
    Publish = #'basic.publish'{exchange = Ex, routing_key = <<"rpc_queue">>},
    Result = amqp_channel:cast(Ch, Publish, #amqp_msg{payload = Payload}),
    %%?log(Result),
    {noreply, State, 1};
                                    

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
