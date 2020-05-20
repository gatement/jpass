-module(jpass_tran_conn).

-behaviour(gen_server).

-export([start_link/1, hand_off/1]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, handle_continue/2, code_change/3, format_status/2, terminate/2]).

-record(state, {socket = undefined}).

-define(SERVER, ?MODULE).
-define(TIMEOUT, 600000).

start_link(Socket) ->
    %io:format("jpass_tran_conn:start_link()~n"),
    gen_server:start_link(?MODULE, [Socket], []).

init([Socket]) ->
    {ok, {{A1, B1, C1, D1}, FromPort}} = inet:peername(Socket),
    {ok, {{A2, B2, C2, D2}, ToPort}} = inet:sockname(Socket),
    io:format("~s ~p TRAN INIT ~p.~p.~p.~p:~p -> ~p.~p.~p.~p:~p~n", [jpass_util:get_datetime_str(), erlang:self(), A1, B1, C1, D1, FromPort, A2, B2, C2, D2, ToPort]),
    {ok, #state{socket=Socket}}.

handle_call(Request, From, State) ->
    io:format("jpass_tran_conn:handle_call() request=~p, from=~p, state=~p~n", [Request, From, State]),
    {reply, my_reply, State, ?TIMEOUT}.
    
handle_cast(Request, State) ->
    Socket = State#state.socket,
    case Request of
        hand_off ->
            ok = inet:setopts(Socket, [{active, true}]);
        _ ->
	    io:format("jpass_tran_conn:handle_cast() request=~p, state=~p~n", [Request, State]),
            ignore
    end,
    {noreply, State, ?TIMEOUT}.

handle_info(Info, State) ->
    Socket = State#state.socket,
    case Info of
        timeout ->
            jpass_util:close_tcp_socket(Socket),
            ok = gen_server:stop(erlang:self());
        {tcp, Socket, DataBin} ->
            {ok, {ToIp, ToPort}} = inet:sockname(State#state.socket),
            io:format("~s ~p TRAN SEND ~p:~p ~p~n", [jpass_util:get_datetime_str(), erlang:self(), ToIp, ToPort, DataBin]),
	    jpass_util:send_req(tran_send, {ToIp, ToPort, DataBin});
        {tcp_closed, _} ->
            jpass_util:close_tcp_socket(Socket),
            ok = gen_server:stop(erlang:self());
        {_AddrInfo, recv, CipherParams} ->
            DataBin = jpass_util:decode(CipherParams),
            %io:format("~s ~p TRAN RECV ~pB~n", [jpass_util:get_datetime_str(), erlang:self(), size(DataBin)]),
            gen_tcp:send(Socket, DataBin);
        {_AddrInfo, stop, _CipherParams} ->
            jpass_util:close_tcp_socket(Socket),
            ok = gen_server:stop(erlang:self());
        _ ->
            io:format("jpass_tran_conn:handle_info() info=~p, state=~p~n", [Info, State])
    end,
    {noreply, State, ?TIMEOUT}.

handle_continue(_Continue, State) ->
    io:format("jpass_tran_conn:handle_continue() continue=~p, state=~p~n", [_Continue, State]),
    {noreply, State, ?TIMEOUT}.
    
code_change(_OldVsn, State, _Extra) -> 
    %io:format("jpass_tran_conn:code_change() oldVsn=~p, state=~p, extra=~p~n", [_OldVsn, State, _Extra]),
    {ok, State, ?TIMEOUT}.

format_status(_Opt, [_PDict, _State]) ->
    %io:format("jpass_tran_conn:format_status() opt=~p, pdict=~p, state=~p~n", [_Opt, _PDict, _State]),
    my_status_not_implemented.

terminate(Reason, _State) ->
    io:format("jpass_tran_conn:terminate() with reason: ~p~n", [Reason]),
    ok.

hand_off(ChildPid) ->
    gen_server:cast(ChildPid, hand_off).
