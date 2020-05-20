-module(jpass_server_dns_conn).

-behaviour(gen_server).

-export([start_link/1]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, handle_continue/2, code_change/3, format_status/2, terminate/2]).

-record(state, {addr_info, socket = undefined}).
-define(TIMEOUT, 600000).

start_link({AddrInfo, Packet}) ->
    %io:format("jpass_server_dns_conn:start_link(), request=~p~n", [{AddrInfo, Packet}]),
    gen_server:start_link(?MODULE, [AddrInfo, Packet], []).

init([AddrInfo, Packet]) ->
    %io:format("~p jpass_server_dns_conn:init() request=~p bytes~n", [AddrInfo, Packet]),
    {ok, #state{addr_info=AddrInfo}, {continue, {setup, Packet}}}.

handle_call(Request, From, State) ->
    io:format("jpass_server_dns_conn:handle_call() request=~p, from=~p, state=~p~n", [Request, From, State]),
    {reply, my_reply, State, ?TIMEOUT}.
    
handle_cast(Request, State) ->
    io:format("jpass_server_dns_conn:handle_cast() request=~p, state=~p~n", [Request, State]),
    {noreply, State, ?TIMEOUT}.

handle_info(Info, State) ->
    Socket = State#state.socket,
    case Info of
        timeout ->
            jpass_util:close_udp_socket(Socket),
            ok = gen_server:stop(erlang:self());
        {udp, Socket, _FromIp, _FromPort, Packet} ->
            %io:format("~s ~p DNS RECV ~pB FROM ~p:~p~n", [jpass_util:get_datetime_str(), erlang:self(), size(Packet), _FromIp, _FromPort]),
            jpass_util:send_res(State#state.addr_info, recv, Packet),
            jpass_util:close_udp_socket(Socket),
            ok = gen_server:stop(erlang:self());
        _ ->
           io:format("jpass_server_dns_conn:handle_info() info=~p, state=~p~n", [Info, State])
    end,
    {noreply, State, ?TIMEOUT}.

handle_continue(Continue, State) ->
    case Continue of
        {setup, Packet} ->
            {ok, Socket} = gen_udp:open(_Port=0, [binary, {active, true}]),

            {ok, Ip} = application:get_env(server_dns_ip),
            {ok, Port} = application:get_env(server_dns_port),
            %io:format("~p jpass_server_dns_conn: send to ~p:~p with ~p bytes ~n", [erlang:system_time(millisecond), Ip, Port, size(Packet)]),
            ok = gen_udp:send(Socket, Ip, Port, Packet),
            {noreply, State#state{socket=Socket}, ?TIMEOUT};
        _ ->
            io:format("jpass_server_dns_conn:handle_continue() continue=~p, state=~p~n", [Continue, State]),
            {noreply, State, ?TIMEOUT}
    end.
    
code_change(_OldVsn, State, _Extra) -> 
    %io:format("jpass_server_dns_conn:code_change() oldVsn=~p, state=~p, extra=~p~n", [_OldVsn, State, _Extra]),
    {ok, State, ?TIMEOUT}.

format_status(_Opt, [_PDict, _State]) ->
    %io:format("jpass_server_dns_conn:format_status() opt=~p, pdict=~p, state=~p~n", [_Opt, _PDict, _State]),
    my_status_not_implemented.

terminate(Reason, _State) ->
    io:format("jpass_server_dns_conn:terminate() with reason: ~p~n", [Reason]),
    ok.
