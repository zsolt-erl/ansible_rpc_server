-module(start).

-export([start/0]).

start()->
    application:start(rfc4627_jsonrpc),
    application:start(rrpc),
    ok.

