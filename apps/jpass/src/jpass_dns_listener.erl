-module(jpass_dns_listener).

-behaviour(gen_server).

-export([start_link/0, send_to_socket/3]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, handle_continue/2, code_change/3, format_status/2, terminate/2]).

-record(state, {server_socket=undefined}).

-define(SERVER, ?MODULE).

start_link() ->
    %io:format("jpass_dns_listener:start_link()~n"),
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

init([]) ->
    %io:format("jpass_dns_listener:init()~n"),
    {ok, #state{}, {continue, {setup}}}.

handle_call(Request, From, State) ->
    io:format("jpass_dns_listener:handle_call() request=~p, from=~p, state=~p~n", [Request, From, State]),
    {reply, my_reply, State}.
    
handle_cast(Request, State) ->
    case Request of
        {send_to_socket, ToIp, ToPort, Packet} ->
            ServerSocket = State#state.server_socket,
            %io:format("jpass_dns_listener:handle_cast() send_to_socket from ~p, to_ip=~p, to_port=~p, packet=~p bytes~n", [ServerSocket, ToIp, ToPort, size(Packet)]),
            gen_udp:send(ServerSocket, ToIp, ToPort, Packet);
        _ ->
            io:format("jpass_dns_listener:handle_cast() request=~p, state=~p~n", [Request, State])
    end,
    {noreply, State}.

handle_info(Info, State) ->
    case Info of
        {udp, _Socket, FromIp, FromPort, Packet} ->
            %io:format("~p jpass_dns_listener: received a DNS request from ~p:~p, packet=~p bytes~n", [erlang:system_time(millisecond), FromIp, FromPort, size(Packet)]),
            jpass_dns_sup:start_child(FromIp, FromPort, Packet);
        _ ->
           io:format("jpass_dns_listener:handle_info() info=~p, state=~p~n", [Info, State])
    end,
   {noreply, State}.

handle_continue(Continue, State) ->
    case Continue of
        {setup} ->
            ServerSocket = init_udp_server(), 
            {noreply, State#state{server_socket = ServerSocket}};
        _ ->
            io:format("jpass_dns_listener:handle_continue() continue=~p, state=~p~n", [Continue, State]),
            {noreply, State}
    end.
    
code_change(_OldVsn, State, _Extra) -> 
    %io:format("jpass_dns_listener:code_change() oldVsn=~p, state=~p, extra=~p~n", [_OldVsn, _State, _Extra]),
    {ok, State}.

format_status(_Opt, [_PDict, _State]) ->
    %io:format("jpass_dns_listener:format_status() opt=~p, pdict=~p, state=~p~n", [_Opt, _PDict, _State]),
    my_status_not_implemented.

terminate(Reason, _State) ->
    io:format("jpass_dns_listener:terminate() with reason: ~p~n", [Reason]),
    ok.

%% return server socket
init_udp_server() ->
    {ok, Ip} = application:get_env(dns_listen_ip),
    {ok, Port} = application:get_env(dns_listen_port),
    {ok, ServerSocket} = gen_udp:open(Port, [binary, {ip, Ip}, {reuseaddr, true}, {active, true}]),

    {ok, ServerIp} = application:get_env(server_dns_ip),
    {ok, ServerPort} = application:get_env(server_dns_port),
    io:format("DNS tunnel ~p:~p ==> ~p:~p~n", [Ip, Port, ServerIp, ServerPort]),

    ServerSocket.

send_to_socket(ToIp, ToPort, Packet) ->
    gen_server:cast(?SERVER, {send_to_socket, ToIp, ToPort, Packet}).
