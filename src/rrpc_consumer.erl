-module(rrpc_consumer).

-behaviour(gen_server).

-include("amqp_client.hrl").
-include("rrpc.hrl").
-include("lib/erlang-rfc4627/include/rfc4627_jsonrpc.hrl").


-define(FUNCTION, begin {current_function, {_M,F,_A}} = process_info(self(), current_function), F end).

%% API
-export([start_link/1]).

%% gen_server callbacks
-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

-record(state, {connection, channel, exchange, service_name, rpc_queue, consumer_tag}).


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
start_link(ServiceName) ->
    gen_server:start_link(?MODULE, ServiceName, []).

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
init(ServiceName) ->
    ?log("starting (for ~p)", [ServiceName]),
    %% connect to broker

    {ok, Connection} = amqp_connection:start(#amqp_params_network{username = ?conf(rmq_username), password = ?conf(rmq_password), host = ?conf(rmq_host) }),
    ?log("connected to broker"),
    {ok, Channel}    = amqp_connection:open_channel(Connection),
    ?log("opened channel"),

    Exchange = <<"test">>,

    %% declare queue

    RPCQueueName = <<"rpc_queue/", ServiceName/binary>>,

    QDeclare = #'queue.declare'{queue = RPCQueueName},
    #'queue.declare_ok'{} = amqp_channel:call(Channel, QDeclare),
    
    Binding = #'queue.bind'{queue = RPCQueueName, exchange = Exchange, routing_key = RPCQueueName},
    #'queue.bind_ok'{} = amqp_channel:call(Channel, Binding),

    %% subscribe rrpc_consumer to queue
    
    Subscribe = #'basic.consume'{queue = RPCQueueName},
    #'basic.consume_ok'{consumer_tag = ConsumerTag} = amqp_channel:call(Channel, Subscribe),
    {ok, #state{connection = Connection, channel = Channel, exchange = Exchange, service_name = ServiceName, rpc_queue = RPCQueueName, consumer_tag = ConsumerTag}}.

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
handle_info(#'basic.consume_ok'{}, State) ->
    ?log("Received: consume_ok"),
    {noreply, State};

handle_info(#'basic.cancel_ok'{}, State) ->
    ?log("Received: cancel_ok"),
    {noreply, State};

handle_info({#'basic.deliver'{delivery_tag = DeliveryTag}, AMQPmsg}, State = #state{channel = Channel}) ->
    ?log("Received delivery record"),

    ?log("Processing message: ~p", [AMQPmsg]),
    Response = process_msg(AMQPmsg, State),

    amqp_channel:cast(Channel, #'basic.ack'{delivery_tag = DeliveryTag}),

    ?log("Acked message"),

    publish_response(AMQPmsg, Response, State),

    {noreply, State};




handle_info(Info, State) ->
    ?log("Received: ~p", [Info]),
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
%%%
%%% Response :: jsonrpc_response() = {result | error, json()} | {result | error, json(), jsonobj()}

process_msg(AMQPmsg, #state{service_name = ServiceName}) ->
    Payload = AMQPmsg#amqp_msg.payload,
    Response = case rfc4627:decode(Payload) of
        {ok, JSON, _Remainder} ->
            ?log("Payload: ~p", [JSON]),
            {obj, Fields} = JSON,
            Id = ?gv("id", Fields),
            Method = ?gv("method", Fields),
            Params = ?gv("params", Fields),
            rrpc_util:invoke(Id, ServiceName, Method, Params);
        _ ->
            ?log("Not a JSON payload"),
            rfc4627_jsonrpc:error_response(500, "Error parsing JSON message", {})
    end,
    ?log("Response: ~p", [Response]),
    Response.

publish_response(AMQPmsg, Response, #state{channel = Channel, exchange = Exchange}) ->
    %% get jsonrpc payload from response (throw away ResponseInfo if exists)
    {_ResultOrError, ResponsePayload} = case Response of
        {RorE, Pl} -> {RorE, Pl};
        {RorE, Pl, _RInfo} -> {RorE, Pl}
    end,
%%    ResponsePayload= {obj,[{version, <<"1.1">>}]},
    ?log("ResponsePayload: ~p", [ResponsePayload]),
    
    CorrelationId = (AMQPmsg#amqp_msg.props)#'P_basic'.correlation_id,
    ReplyTo = (AMQPmsg#amqp_msg.props)#'P_basic'.reply_to,
    ?log("(~p) got routing data", [?FUNCTION]),
    ?log("correlation id: ~p", [CorrelationId]),
    ?log("reply_to: ~p", [ReplyTo]),

    QDeclare = #'queue.declare'{queue = ReplyTo},
    #'queue.declare_ok'{} = amqp_channel:call(Channel, QDeclare),
    
    Binding = #'queue.bind'{queue = ReplyTo, exchange = Exchange, routing_key = ReplyTo},
    #'queue.bind_ok'{} = amqp_channel:call(Channel, Binding),

    BinEncodedResponsePayload = list_to_binary( rfc4627:encode(ResponsePayload) ),
    ?log("encoded payload: ~p", [BinEncodedResponsePayload]),

%%     Props = #'P_basic'{content_type = <<"application/octet-stream">>},
    Props = #'P_basic'{correlation_id = CorrelationId, content_type = <<"application/octet-stream">>},
    Publish = #'basic.publish'{exchange = Exchange,
                               routing_key = ReplyTo,
                               mandatory = true},
    Result = amqp_channel:cast(Channel, Publish, #amqp_msg{props = Props, payload = BinEncodedResponsePayload}),

%%     %%%%% this is from test_publisher and is supposed to be working
%%     Props = #'P_basic'{content_type = <<"application/octet-stream">>},
%%     Publish = #'basic.publish'{exchange = Exchange,
%%                                routing_key = <<"whatever">>,
%%                                mandatory = true},
%%     Result = amqp_channel:cast(Channel, Publish, #amqp_msg{props = Props, payload = BinEncodedResponsePayload}),





    ?log("(~p) Result: ~p", [?FUNCTION, Result]),
    ok.
