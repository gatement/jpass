-module(jpass_tran_listener).

-behaviour(gen_server).

-export([start_link/0]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, handle_continue/2, code_change/3, format_status/2, terminate/2]).

-record(state, {server_socket=undefined}).

-define(SERVER, ?MODULE).

start_link() ->
    %io:format("jpass_tran_listener:start_link()~n"),
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

init([]) ->
    %io:format("jpass_tran_listener:init()~n"),
    {ok, #state{}, {continue, {setup}}}.

handle_call(Request, From, State) ->
    io:format("jpass_tran_listener:handle_call() request=~p, from=~p, state=~p~n", [Request, From, State]),
    {reply, my_reply, State, 0}.
    
handle_cast(Request, State) ->
    io:format("jpass_tran_listener:handle_cast() request=~p, state=~p~n", [Request, State]),
    {noreply, State, 0}.

handle_info(Info, State) ->
    case Info of
        timeout -> 
            %io:format("waiting for TRAN conn comming...~n"),
            {ok, Socket} = gen_tcp:accept(State#state.server_socket),
            %io:format("a new TRAN conn connected.~n"),
            {ok, ChildPid} = jpass_tran_sup:start_child(Socket),
            ok = gen_tcp:controlling_process(Socket, ChildPid),
            jpass_tran_conn:hand_off(ChildPid),
            {noreply, State, 0};
       {tcp_closed, _} ->
	   {noreply, State, 0};
        _ ->
           io:format("jpass_tran_listener:handle_info() info=~p, state=~p~n", [Info, State]),
	   {noreply, State, 0}
    end.

handle_continue(Continue, State) ->
    case Continue of
        {setup} -> 
            ServerSocket = init_tcp_server(), 
            {noreply, State#state{server_socket = ServerSocket}, 0};
        _ ->
            io:format("jpass_tran_listener:handle_continue() continue=~p, state=~p~n", [Continue, State]),
            {noreply, State, 0}
    end.
    
code_change(_OldVsn, State, _Extra) -> 
    %io:format("jpass_tran_listener:code_change() oldVsn=~p, state=~p, extra=~p~n", [_OldVsn, _State, _Extra]),
    {ok, State, 0}.

format_status(_Opt, [_PDict, _State]) ->
    %io:format("jpass_tran_listener:format_status() opt=~p, pdict=~p, state=~p~n", [_Opt, _PDict, _State]),
    my_status_not_implemented.

terminate(Reason, _State) ->
    io:format("jpass_tran_listener:terminate() with reason: ~p~n", [Reason]),
    ok.

%% return server socket
init_tcp_server() ->
    {ok, Ip} = application:get_env(tran_listen_ip),
    {ok, Port} = application:get_env(tran_listen_port),

    {ok, ServerSocket} = gen_tcp:listen(Port, [binary, {packet, 0}, {ip, Ip}, {reuseaddr, true}, {active, false}]),

    Opt = {raw, 0, 19, <<1:32/native>>},
    inet:setopts(ServerSocket, [Opt]),
    {ok, [Opt]}=inet:getopts(ServerSocket,[{raw, 0, 19, 4}]),

    io:format("TRAN tunnel on ~p:~p~n", [Ip, Port]),
    ServerSocket.
