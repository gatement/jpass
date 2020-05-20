-module(jpass_server_tran_conn).

-behaviour(gen_server).

-export([start_link/2]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, handle_continue/2, code_change/3, format_status/2, terminate/2]).

-record(state, {addr_info, socket = undefined}).

-define(TIMEOUT, 600000).

start_link(ChildName, {AddrInfo, {ToIp, ToPort, DataBin}}) ->
    %io:format("jpass_server_tran_conn:start_link(), child_name=~p, request=~p~n", [ChildName, {AddrInfo, {ToIp, ToPort, DataBin}}]),
    gen_server:start_link({local, ChildName}, ?MODULE, [AddrInfo, {ToIp, ToPort, DataBin}], []).

init([AddrInfo, {ToIp, ToPort, DataBin}]) ->
    %io:format("jpass_server_tran_conn:init() request=~p~n", [{AddrInfo, {ToIp, ToPort, DataBin}}]),
    {ok, #state{addr_info=AddrInfo}, {continue, {setup, {ToIp, ToPort, DataBin}}}}.

handle_call(Request, From, State) ->
    io:format("jpass_server_tran_conn:handle_call() request=~p, from=~p, state=~p~n", [Request, From, State]),
    {reply, my_reply, State, ?TIMEOUT}.
    
handle_cast(Request, State) ->
    io:format("jpass_server_tran_conn:handle_cast() request=~p, state=~p~n", [Request, State]),
    {noreply, State, ?TIMEOUT}.

handle_info(Info, State) ->
    Socket = State#state.socket,
    case Info of
        timeout ->
            jpass_util:close_tcp_socket(Socket),
            ok = gen_server:stop(erlang:self());
        {send, DataBin} ->
            %io:format("send to ~p: ~p~n", [Socket, DataBin]),
            ok = gen_tcp:send(Socket, DataBin);
        {tcp, Socket, DataBin} ->
            %io:format("~s ~p TRAN RECV ~pB~n", [jpass_util:get_datetime_str(), erlang:self(), size(DataBin)]),
            jpass_util:send_res(State#state.addr_info, recv, DataBin);
        {tcp_closed, _} ->
            jpass_util:send_res(State#state.addr_info, stop, undefined),
            jpass_util:close_tcp_socket(Socket),
            ok = gen_server:stop(erlang:self());
        _ ->
           io:format("jpass_server_tran_conn:handle_info() info=~p, state=~p~n", [Info, State])
    end,
    {noreply, State, ?TIMEOUT}.

handle_continue(Continue, State) ->
    case Continue of
        {setup, {ToIp, ToPort, DataBin}} ->
            try
                {ok, Socket} = gen_tcp:connect(ToIp, ToPort, [binary, {packet, 0}, {active, true}]),
                %io:format("send to ~p: ~p~n", [Socket, DataBin]),
                ok = gen_tcp:send(Socket, DataBin),
                {noreply, State#state{socket=Socket}, ?TIMEOUT}
            catch
                _Type:Error ->
                    io:format("TRAN ~p:~p ~p~n", [ToIp, ToPort, Error]),
                    jpass_util:send_res(State#state.addr_info, stop, undefined),
                    ok = gen_server:stop(erlang:self()),
                    {noreply, State}
            end;
        _ ->
            io:format("jpass_server_tran_conn:handle_continue() continue=~p, state=~p~n", [Continue, State]),
            {noreply, State, ?TIMEOUT}
    end.
    
code_change(_OldVsn, State, _Extra) -> 
    %io:format("jpass_server_tran_conn:code_change() oldVsn=~p, state=~p, extra=~p~n", [_OldVsn, State, _Extra]),
    {ok, State, ?TIMEOUT}.

format_status(_Opt, [_PDict, _State]) ->
    %io:format("jpass_server_tran_conn:format_status() opt=~p, pdict=~p, state=~p~n", [_Opt, _PDict, _State]),
    my_status_not_implemented.

terminate(Reason, _State) ->
    io:format("jpass_server_tran_conn:terminate() with reason: ~p~n", [Reason]),
    ok.
