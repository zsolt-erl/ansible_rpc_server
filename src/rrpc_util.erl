-module(rrpc_util).

-export( [register_service_module/1, invoke/4, create_procedure_description/1, create_service_description/1] ).
-compile([export_all]).

-include("amqp_client.hrl").
-include("rrpc.hrl").
-include("lib/erlang-rfc4627/include/rfc4627_jsonrpc.hrl").

-define(CHILD(I, Type), {I, {I, start_link, []}, permanent, 5000, Type, [I]}).



i()->invoke(12345, <<"ansible_connector">>, <<"sasldb_list">>, ["qa1s1.smtprelay.auth"]).
d(Service) -> invoke(12345, Service, <<"system.describe">>, []).
c()->
    register_service_module(service_ansible),
    {result, Desc, _} = d(<<"ansible_connector">>),
    Desc.




register_service_module(ModuleName) when is_list(ModuleName) -> register_service_module(list_to_atom(ModuleName));
register_service_module(ModuleName) ->
    %% - start service
    %% - register service with rfc4627_jsonrpc service registry
    %% - start a consumer for the service 

    %% start service

    ChildSpec = ?CHILD(ModuleName, worker),
    supervisor:start_child(rrpc_services_sup, ChildSpec),
    
     
    %% register service
    
    ServiceDesc = ModuleName:rpc_service_description(),
    ServiceName = ServiceDesc#service.name,
    {status, ServicePid, _Module, _Items} = sys:get_status(ModuleName),
    rfc4627_jsonrpc:register_service(ServicePid, ServiceDesc),
    ?log("registered service: ~s", [ServiceName]),

    %% start consumer

    supervisor:start_child(rrpc_consumer_sup, [ServiceName]).





invoke(RequestId, ServiceName, ServiceProc, Args) when is_atom(ServiceName) -> 
    invoke(RequestId, list_to_binary(atom_to_list(ServiceName)), ServiceProc, Args);

invoke(RequestId, ServiceName, ServiceProc, Args) when is_atom(ServiceProc) -> 
    invoke(RequestId, ServiceName, list_to_binary(atom_to_list(ServiceProc)), Args);

invoke(RequestId, ServiceName, ServiceProc, Args) when is_list(ServiceName) -> 
    invoke(RequestId, list_to_binary(ServiceName), ServiceProc, Args);

invoke(RequestId, ServiceName, ServiceProc, Args) when is_list(ServiceProc) -> 
    invoke(RequestId, ServiceName, list_to_binary(ServiceProc), Args);

invoke(RequestId, ServiceName, ServiceProc, Args) -> 
    case rfc4627_jsonrpc:lookup_service(ServiceName) of
        not_found ->
            rfc4627_jsonrpc:error_response(404, "Service not found", {});
    ServiceRec ->
            PostOrGet = post,
            RequestInfo = {obj, []},
            EndpointAddress = undefined,
            Method = ServiceProc,
            Timeout = 5*60*1000,

            Response = rfc4627_jsonrpc:invoke_service_method(ServiceRec, RequestId, PostOrGet, RequestInfo, EndpointAddress, Method, Args, Timeout),

            ?log("SERVICE RESPONSE: ~p", [Response]),
            Response
    end.

create_procedure_description(PropList) ->
    ProcParams = [ {?gv(name, OneParamPropList), ?gv(type, OneParamPropList)} || OneParamPropList <- ?gv(params, PropList) ],

    Rec1 = case ?gv(return, PropList) of
        undefined -> rfc4627_jsonrpc:proc( ?gv(name, PropList), ProcParams );
        Ret       -> 
            rfc4627_jsonrpc:proc2( ?gv(name, PropList), ProcParams, {?gv(name, Ret), ?gv(type, Ret)} )
    end,

    Rec2 = Rec1#service_proc{summary    = normalize( ?gv(summary, PropList) ), 
                             help       = normalize( ?gv(help, PropList) ),
                             idempotent = ?gv(get_allowed, PropList)},
    Rec2.


create_service_description(PropList) ->
    ServiceRec1 = rfc4627_jsonrpc:service(
            ?gv(name,    PropList),
            ?gv(id,      PropList),
            ?gv(version, PropList),
            ?gv(procs  , PropList)),

    ServiceRec2 = ServiceRec1#service{summary = normalize( ?gv(summary, PropList) ),
                                      help    = normalize( ?gv(help, PropList) )},

    ServiceRec2.




normalize(X) when is_list(X) -> list_to_binary(X);
normalize(X) -> X.


os_cmd(Action, Cmd) ->
    io:format("~ts starting... Shell command: ~s~n", [Action, Cmd]),
    try erlang:open_port({spawn, Cmd}, [exit_status, stderr_to_stdout]) of
        Port -> 
            os_cmd_exitstatus_loop(Action, Port, [])
    catch
        _:Reason ->
            case Reason of
                badarg ->
                    Message = "Bad input arguments";
                system_limit ->
                    Message = "All available ports in the Erlang emulator are in use";
                _ ->
                    Message = file:format_error(Reason)
            end,
            io:format("~ts: shell command error: ~ts~n", [Action, Message]),
            error
    end.

os_cmd_exitstatus_loop(Action, Port, Acc) ->
    receive
        {Port, {data, Data}} ->
            io:format("~ts... Shell output: ~ts~n", [Action, Data]),
            os_cmd_exitstatus_loop(Action, Port, Acc++[Data]);
        {Port, {exit_status, 0}} ->
            io:format("~ts finished successfully~n", [Action]),
            Acc;
        {Port, {exit_status, Status}} ->
            io:format("~ts failed with exit status ~p~n", [Action, Status]),
            error;
        {'EXIT', Port, Reason} ->
            io:format("~ts failed with port exit: reason ~ts~n", [Action, file:format_error(Reason)]),
            error
    end.


load_yaml( FileName ) ->
    Port = open_port({spawn, "python -u scripts/yaml_handler.py"}, [{packet, 4}, binary ]),
    port_command(Port, term_to_binary(  {load_yaml, FileName}  )),
    receive
        {Port, {data, Data}} ->
            port_close(Port),
            binary_to_term(Data)
    after
        500 ->
            {error, timeout}
    end.

dump_yaml( FileName, YAMLTerm ) ->
    Port = open_port({spawn, "python -u scripts/yaml_handler.py"}, [{packet, 4}, binary ]),
    port_command(Port, term_to_binary(  {dump_yaml, FileName, YAMLTerm}  )),
    receive
        {Port, {data, Data}} ->
            port_close(Port),
            binary_to_term(Data)
    after
        500 ->
            {error, timeout}
    end.

