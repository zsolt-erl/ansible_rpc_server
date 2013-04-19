%% JSON-RPC service implementation
%%
%% handle_call/3 needs to handle jsonrpc messages of the format:
%%      Request = {jsonrpc, Method, RequestInfo, Args}
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



-module(service_ansible).

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

-export([increment/1, sasldb_list/1]).

-define(ltb(X), list_to_binary(X)).


-record(state, {counter = 0}).


%% http://tonyg.github.com/erlang-rfc4627/doc/
rpc_service_description()->
    IncrementProc = rrpc_util:create_procedure_description([
                {name,        "increment"},
                {summary,     "This is a simple procedure to increment a counter"},
                {help,        "No help here. You're on your own."},
                {get_allowed, false},
                {params, [
                        [{name, "step"}, {type, num}]
                        ]}
                ]),
    SASLdbListProc = rrpc_util:create_procedure_description([
                {name,        "sasldb_list"},
                {summary,     "Returns a list of usernames/passwords in a sasl database for each contacted host"},
                {help,        ""},
                {get_allowed, false},
                {params, [
                        [{name, "selector"}, {type, str}]
                        ]},
                {return, [{name, "credentials"}, {type, obj}]}
                ]),

    AddAuthUserProc = rrpc_util:create_procedure_description([
                {name,        "add_auth_user"},
                {summary,     "Adds new user to ansible vars file and syncs sasldb to it (Postfix user provisioning)"},
                {help,        ""},
                {get_allowed, false},
                {params, [
                        [{name, "selector"}, {type, str}],
                        [{name, "id"},       {type, str}],
                        [{name, "username"}, {type, str}],
                        [{name, "password"}, {type, str}]
                        ]}
                ]),

    DeleteAuthUserProc = rrpc_util:create_procedure_description([
                {name,        "delete_auth_user"},
                {summary,     "Deletes a user from ansible vars file and syncs sasldb to it (Postfix user provisioning)"},
                {help,        ""},
                {get_allowed, false},
                {params, [
                        [{name, "selector"}, {type, str}],
                        [{name, "id"},       {type, str}]
                        ]}
                ]),

    Service = rrpc_util:create_service_description([
                {name,    "ansible_connector"},
                {id,      "mz/x"},
                {version, "0.1"},
                {summary, "Runs Ansible playbooks and returns the output if there's any"},
                {help,    ""},
                {procs, [
                        SASLdbListProc, 
                        AddAuthUserProc,
                        DeleteAuthUserProc,
                        IncrementProc]}
                ]),
    Service.


%% TODO:
%% AddUserProc, DeleteUserProc
%%
%% AddUserProc:
%% - load YAML file containing variables
%% - insert new username,password  (or list of usernames/passwords)
%% - write updated YAML file
%% - run sasldb playbook sync operation
%% FIXME: what happens when dict is empty after deletion
%% FIXME: what happens when the list of whitelisted IPs is empty
%%
%% TODO: ReadVarProc : read a variable from the yaml variables file


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

sasldb_list(Selector) ->
    ?log("CALLED SASLDB_LIST"),
    Reply = gen_server:call(?MODULE, {jsonrpc, <<"sasldb_list">>, [], [Selector]}, 5*60*1000),
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
handle_call({jsonrpc, <<"increment">>, _RequestInfo, [Step]}, _From, State = #state{counter = Counter}) ->
    NewCounter = Counter + Step,
    Reply = {result, {obj, [{"Counter", NewCounter}]}},
    {reply, Reply, State#state{counter = NewCounter}};


handle_call({jsonrpc, <<"sasldb_list">>, _RequestInfo, [Selector]}, _From, State ) ->
    DecodedJSONReply = rrpc_apr:run(?conf(ansible_data_dir)++"/playbooks/postfix.pb/sasldb_list.yml", Selector, ""),   %% last argument is options for ansible-playbook eg.: specifying tags
    Reply = {result, DecodedJSONReply},
    {reply, Reply, State};


%% AddUserProc:
%% - load YAML file containing variables
%% - insert new username,password  (or list of usernames/passwords)
%% - write updated YAML file
%% - run sasldb playbook sync operation
handle_call({jsonrpc, <<"add_auth_user">>, _RequestInfo, [Selector, UserID, Username, Password]}, _From, State ) ->
    %% read vars file
    VarsFile = ?ltb(?conf(ansible_data_dir)++"/playbooks/postfix.pb/vars/group_vars/"++Selector),
    Vars = rrpc_util:load_yaml(VarsFile),

    %% get current users
    AuthorizedUsers = case proplists:get_value(<<"authorized_users">>, Vars) of
        undefined -> [];
        AU ->        AU
    end,

    %% update vars with new user
    NewUser = {?ltb(UserID), [{<<"username">>, ?ltb(Username)}, {<<"password">>, ?ltb(Password)}]},
    NewAU = lists:keystore(?ltb(UserID), 1, AuthorizedUsers, NewUser),
    NewVars = lists:keystore(<<"authorized_users">>, 1, Vars, {<<"authorized_users">>, NewAU}),

    %% dump updated variables to vars file
    <<"done">> = rrpc_util:dump_yaml(VarsFile, NewVars),

    %% execute sync playbook
    DecodedJSONReply = rrpc_apr:run(?conf(ansible_data_dir)++"/playbooks/postfix.pb/sasldb_sync.yml", Selector, ""),   %% last argument is options for ansible-playbook eg.: specifying tags
    Reply = {result, DecodedJSONReply},

    {reply, Reply, State};



handle_call({jsonrpc, <<"delete_auth_user">>, _RequestInfo, [Selector, UserID]}, _From, State ) ->
    %% read vars file
    VarsFile = ?ltb(?conf(ansible_data_dir)++"/playbooks/postfix.pb/vars/group_vars/"++Selector),
    Vars = rrpc_util:load_yaml(VarsFile),

    %% get current users
    AuthorizedUsers = case proplists:get_value(<<"authorized_users">>, Vars) of
        undefined -> [];
        AU ->        AU
    end,

    %%  delete user from vars
    NewAU = lists:keydelete(?ltb(UserID), 1, AuthorizedUsers),
    NewVars = lists:keystore(<<"authorized_users">>, 1, Vars, {<<"authorized_users">>, NewAU}),

    %% dump updated variables to vars file
    <<"done">> = rrpc_util:dump_yaml(VarsFile, NewVars),

    %% execute sync playbook
    DecodedJSONReply = rrpc_apr:run(?conf(ansible_data_dir)++"/playbooks/postfix.pb/sasldb_sync.yml", Selector, ""),   %% last argument is options for ansible-playbook eg.: specifying tags
    Reply = {result, DecodedJSONReply},

    {reply, Reply, State};



handle_call(Request = {jsonrpc, _Method, _RequestInfo, _Args}, _From, State) ->
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
