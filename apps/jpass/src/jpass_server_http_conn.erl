-module(jpass_server_http_conn).

-behaviour(gen_server).

-export([start_link/2]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, handle_continue/2, code_change/3, format_status/2, terminate/2]).

-record(state, {addr_info, host, port, socket = undefined}).

-define(TIMEOUT, 600000).

start_link(ChildName, {AddrInfo, {Host, Port, DataBin}}) ->
    %io:format("jpass_server_http_conn:start_link(), child_name=~p, request=~p~n", [ChildName, {AddrInfo, {Host, Port, DataBin}}]),
    gen_server:start_link({local, ChildName}, ?MODULE, [AddrInfo, {Host, Port, DataBin}], []).

init([AddrInfo, {Host, Port, DataBin}]) ->
    %io:format("jpass_server_http_conn:init() request=~p~n", [AddrInfo, {Host, Port, size(DataBin)}]),
    io:format("~s ~p HTTP ~s:~p INIT~n", [jpass_util:get_datetime_str(), self(), Host, Port]),
    {ok, #state{addr_info=AddrInfo}, {continue, {setup, {Host, Port, DataBin}}}}.

handle_call(Request, From, State) ->
    io:format("jpass_server_http_conn:handle_call() request=~p, from=~p, state=~p~n", [Request, From, State]),
    {reply, my_reply, State, ?TIMEOUT}.
    
handle_cast(Request, State) ->
    io:format("jpass_server_http_conn:handle_cast() request=~p, state=~p~n", [Request, State]),
    {noreply, State, ?TIMEOUT}.

handle_info(Info, State) ->
    Socket = State#state.socket,
    Host = State#state.host,
    Port = State#state.port,
    case Info of
        timeout ->
            jpass_util:close_tcp_socket(Socket),
            ok = gen_server:stop(erlang:self()),
	    io:format("~s ~p HTTP ~s:~p DOWN~n", [jpass_util:get_datetime_str(), self(), Host, Port]);
        {send, DataBin} ->
            %io:format("send to ~p: ~p~n", [Socket, DataBin]),
            ok = gen_tcp:send(Socket, DataBin),
	    io:format("~s ~p HTTP ~s:~p SENT~n", [jpass_util:get_datetime_str(), self(), Host, Port]),
            ok;
        {tcp, Socket, DataBin} ->
	    io:format("~s ~p HTTP ~s:~p RECV~n", [jpass_util:get_datetime_str(), self(), Host, Port]),
            jpass_util:send_res(State#state.addr_info, recv, DataBin);
        {tcp_closed, _} ->
	    io:format("~s ~p HTTP ~s:~p CLSD~n", [jpass_util:get_datetime_str(), self(), Host, Port]),
            jpass_util:send_res(State#state.addr_info, stop, undefined),
            jpass_util:close_tcp_socket(Socket),
            ok = gen_server:stop(erlang:self());
        _ ->
           io:format("jpass_server_http_conn:handle_info() info=~p, state=~p~n", [Info, State])
    end,
    {noreply, State, ?TIMEOUT}.

handle_continue(Continue, State) ->
    case Continue of
        {setup, {Host, Port, DataBin}} ->
            try
                {ok, Socket} = gen_tcp:connect(Host, Port, [binary, {packet, 0}, {active, true}]),
                io:format("~s ~p HTTP ~s:~p CONN~n", [jpass_util:get_datetime_str(), self(), Host, Port]),
                case DataBin of
                    <<>> ->
                        ignore;
                    _ ->
                        %io:format("send to ~p: ~p~n", [Socket, DataBin]),
                        ok = gen_tcp:send(Socket, DataBin)
                end,
                {noreply, State#state{socket = Socket, host = Host, port = Port}, ?TIMEOUT}
            catch
                _Type:Error ->
                    io:format("ERROR: HTTP ~s:~p ~p~n", [Host, Port, Error]),
                    jpass_util:send_res(State#state.addr_info, stop, undefined),
                    ok = gen_server:stop(erlang:self()),
                    {noreply, State}
            end;
        _ ->
            io:format("jpass_server_http_conn:handle_continue() continue=~p, state=~p~n", [Continue, State]),
            {noreply, State, ?TIMEOUT}
    end.
    
code_change(_OldVsn, State, _Extra) -> 
    %io:format("jpass_server_http_conn:code_change() oldVsn=~p, state=~p, extra=~p~n", [_OldVsn, State, _Extra]),
    {ok, State, ?TIMEOUT}.

format_status(_Opt, [_PDict, _State]) ->
    %io:format("jpass_server_http_conn:format_status() opt=~p, pdict=~p, state=~p~n", [_Opt, _PDict, _State]),
    my_status_not_implemented.

terminate(Reason, _State) ->
    io:format("jpass_server_http_conn:terminate() with reason: ~p~n", [Reason]),
    ok.
