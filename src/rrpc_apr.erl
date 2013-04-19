-module(rrpc_apr).
%% ansible playbook runner

-behaviour(gen_server).

%% API
-export([start_link/0, run/3, t/0]).


%% gen_server callbacks
-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).


-define(ANSIBLE, "scripts/rrpc_apr.py").

-record(state, {}).



t()->rrpc_apr:run("/home/zsolt/ansible-data/playbooks/postfix.pb/sasldb_list.yml", "qa1s1.smtprelay.auth", "").


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
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).



run(Playbook, Selector, Options)->
    gen_server:call(?MODULE, {run_playbook, Playbook, Selector, Options}, 5*60*1000).    %% 5 min timeout, a playbook can take a long time to run,   TODO: make it async


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
handle_call( {run_playbook, Playbook, Selector, Options}, _From, State) ->
    CmdLine = lists:flatten(
            string:join( [
                    ?ANSIBLE,
                    Playbook,
                    "-e \"selector="++Selector++"\"",
                    Options
                    ]," ")),


    io:format("~n~nCOMMAND LINE:~p~n~n", [CmdLine]),

    StdOut = rrpc_util:os_cmd( apr, CmdLine ),
    LastLine = hd(lists:reverse(StdOut)),
    PlaybookOut = case re:run(LastLine, "PLAYBOOK_OUT\n*(.*)", [{capture, all, list}, dotall] ) of
        {match, List} -> 
            hd( lists:reverse(List) );
        nomatch -> "{}"
    end,

    {ok, Value, _Remaining} = rfc4627:decode_noauto(PlaybookOut),
    Reply = Value,

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
