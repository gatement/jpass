-module(jpass_app).

-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    %io:format("jpass_app:start()~n"),
    jpass_sup:start_link().

stop(_State) ->
    %io:format("jpass_app:stop()~n"),
    ok.
