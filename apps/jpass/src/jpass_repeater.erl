-module(jpass_repeater).

-behaviour(gen_server).

-export([start_link/0]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, handle_continue/2, code_change/3, format_status/2, terminate/2]).

-record(state, {}).

-define(SERVER, ?MODULE).
-define(JPASS_SERVER, jpass_server).

start_link() ->
    %io:format("jpass_repeater:start_link()~n"),
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

init([]) ->
    %io:format("jpass_repeater:init()~n"),
    {ok, #state{}}.

handle_call(Request, From, State) ->
    io:format("jpass_repeater:handle_call() request=~p, from=~p, state=~p~n", [Request, From, State]),
    {reply, my_reply, State}.
    
handle_cast({{FromPid, undefined}, Cmd, Params}, State) ->
    %io:format("~s REQ ~p~n", [jpass_util:get_datetime_str(), Cmd]),
    io:format("S"),
    Request = {{FromPid, erlang:self()}, Cmd, Params},
    {ok, ServerRef} = application:get_env(server_ref),
    gen_server:cast(ServerRef, Request),
    {noreply, State}.

handle_info(Info, State) ->
   case Info of
       {{FromPid, RepeaterPid}, Cmd, Params} ->
           %io:format("~s RES ~p~n", [jpass_util:get_datetime_str(), Cmd]),
           io:format("R"),

           Response = {{FromPid, RepeaterPid}, Cmd, Params},
           FromPid ! Response;
       _ ->
           io:format("jpass_repeater:handle_info() info=~p, state=~p~n", [Info, State])
   end,
   {noreply, State}.

handle_continue(_Continue, State) ->
    io:format("jpass_repeater:handle_continue() continue=~p, state=~p~n", [_Continue, State]),
    {noreply, State}.
    
code_change(_OldVsn, State, _Extra) -> 
    %io:format("jpass_repeater:code_change() oldVsn=~p, state=~p, extra=~p~n", [_OldVsn, _State, _Extra]),
    {ok, State}.

format_status(_Opt, [_PDict, _State]) ->
    %io:format("jpass_repeater:format_status() opt=~p, pdict=~p, state=~p~n", [_Opt, _PDict, _State]),
    my_status_not_implemented.

terminate(Reason, _State) ->
    io:format("jpass_repeater:terminate() with reason: ~p~n", [Reason]),
    ok.

