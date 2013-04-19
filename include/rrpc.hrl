-define(log(Msg), io:format("[~-20.20s] ~p~n", [?MODULE, Msg])).
-define(log(Format, Args), io:format("[~-20.20s] "++Format++"~n", [?MODULE]++Args)).

-define(gv(Key, List), proplists:get_value(Key, List)).


-define( conf(Par),
        fun()->
            {ok, Val}=application:get_env(rrpc, Par),
            Val
    end()
       ).

-define(ld(Arg),        lager:debug(Arg)).
-define(ld(Arg1, Arg2), lager:debug(Arg1, Arg2)).

-define(li(Arg),        lager:info(Arg)).
-define(li(Arg1, Arg2), lager:info(Arg1, Arg2)).

-define(ln(Arg),        lager:notice(Arg)).
-define(ln(Arg1, Arg2), lager:notice(Arg1, Arg2)).

-define(lw(Arg),        lager:warning(Arg)).
-define(lw(Arg1, Arg2), lager:warning(Arg1, Arg2)).

-define(le(Arg),        lager:error(Arg)).
-define(le(Arg1, Arg2), lager:error(Arg1, Arg2)).

