-module(jpass_dns_conn).

-behaviour(gen_server).

-export([start_link/3]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, handle_continue/2, code_change/3, format_status/2, terminate/2]).

-record(state, {from_ip, from_port}).

-define(SERVER, ?MODULE).
-define(TIMEOUT, 600000).

start_link(FromIp, FromPort, Packet) ->
    %io:format("jpass_dns_conn:start_link()~n"),
    gen_server:start_link(?MODULE, [FromIp, FromPort, Packet], []).

init([FromIp, FromPort, Packet]) ->
    %io:format("~p jpass_dns_conn:init() from=~p, to=~p, packet=~p bytes~n", [erlang:system_time(millisecond), FromIp, FromPort, size(Packet)]),
    {A, B, C, D} = FromIp,
    io:format("~s ~p DNS from ~p.~p.~p.~p:~p~n", [jpass_util:get_datetime_str(), erlang:self(), A, B, C, D, FromPort]),

    {ok, #state{from_ip=FromIp, from_port=FromPort}, {continue, {send, Packet}}}.

handle_call(Request, From, State) ->
    io:format("jpass_dns_conn:handle_call() request=~p, from=~p, state=~p~n", [Request, From, State]),
    {reply, my_reply, State, ?TIMEOUT}.
    
handle_cast(Request, State) ->
    io:format("jpass_dns_conn:handle_cast() request=~p, state=~p~n", [Request, State]),
    {noreply, State, ?TIMEOUT}.

handle_info(Info, State) ->
    case Info of
        timeout ->
            ok = gen_server:stop(erlang:self());
        {_AddrInfo, recv, CipherParams} ->
            Packet = jpass_util:decode(CipherParams),
            %io:format("~s ~p DNS RECV ~pB~n", [jpass_util:get_datetime_str(), erlang:self(), size(Packet)]),
            #state{from_ip=FromIp, from_port=FromPort} = State,
            jpass_dns_listener:send_to_socket(FromIp, FromPort, Packet),
            %io:format("~p jpass_dns_conn:handle_info() received and send to ~p:~p with ~p bytes ~n", [erlang:system_time(millisecond), FromIp, FromPort, size(Packet)]),
            ok = gen_server:stop(erlang:self());
        _ ->
           io:format("jpass_dns_conn:handle_info() info=~p, state=~p~n", [Info, State])
    end,
    {noreply, State, ?TIMEOUT}.

handle_continue(Continue, State) ->
    case Continue of
        {send, Packet} ->
            %io:format("~p jpass_dns_conn: send[~p]: ~p bytes~n", [erlang:system_time(millisecond), size(Packet), size(Packet)]),
            jpass_util:send_req(dns_send, Packet);
        _ ->
            io:format("jpass_dns_conn:handle_continue() continue=~p, state=~p~n", [Continue, State])
    end,
    {noreply, State, ?TIMEOUT}.
    
code_change(_OldVsn, State, _Extra) -> 
    %io:format("jpass_dns_conn:code_change() oldVsn=~p, state=~p, extra=~p~n", [_OldVsn, State, _Extra]),
    {ok, State, ?TIMEOUT}.

format_status(_Opt, [_PDict, _State]) ->
    %io:format("jpass_dns_conn:format_status() opt=~p, pdict=~p, state=~p~n", [_Opt, _PDict, _State]),
    my_status_not_implemented.

terminate(Reason, _State) ->
    io:format("jpass_dns_conn:terminate() with reason: ~p~n", [Reason]),
    ok.
