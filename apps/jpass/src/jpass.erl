-module(jpass).

-export([start/0]).

start() ->
    ok = application:start(jpass, permanent).
