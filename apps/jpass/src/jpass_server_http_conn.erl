-module(jpass_server_http_conn).

-behaviour(gen_server).

-export([start_link/2]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, handle_continue/2, code_change/3, format_status/2, terminate/2]).

-record(state, {addr_info, socket}).

start_link(ChildName, {AddrInfo, {Host, Port, DataBin}}) ->
    %io:format("jpass_server_http_conn:start_link(), child_name=~p, request=~p~n", [ChildName, {AddrInfo, {Host, Port, DataBin}}]),
    gen_server:start_link({local, ChildName}, ?MODULE, [AddrInfo, {Host, Port, DataBin}], []).

init([AddrInfo, {Host, Port, DataBin}]) ->
    %io:format("jpass_server_http_conn:init() request=~p~n", [AddrInfo, {Host, Port, DataBin}]),
    {ok, #state{addr_info=AddrInfo}, {continue, {setup, {Host, Port, DataBin}}}}.

handle_call(Request, From, State) ->
    io:format("jpass_server_http_conn:handle_call() request=~p, from=~p, state=~p~n", [Request, From, State]),
    {reply, my_reply, State}.
    
handle_cast(Request, State) ->
    io:format("jpass_server_http_conn:handle_cast() request=~p, state=~p~n", [Request, State]),
    {noreply, State}.

handle_info(Info, State) ->
    Socket = State#state.socket,
    case Info of
        {send, DataBin} ->
            %io:format("send to ~p: ~p~n", [Socket, DataBin]),
            ok = gen_tcp:send(Socket, DataBin);
        {tcp, Socket, DataBin} ->
            %io:format("~s ~p HTTP RECV ~pB~n", [jpass_util:get_datetime_str(), erlang:self(), size(DataBin)]),
            jpass_util:send_res(State#state.addr_info, recv, DataBin);
        {tcp_closed, _} ->
            jpass_util:send_res(State#state.addr_info, stop, undefined),
            ok = gen_tcp:close(Socket),
            ok = gen_server:stop(erlang:self());
        _ ->
           io:format("jpass_server_http_conn:handle_info() info=~p, state=~p~n", [Info, State])
    end,
    {noreply, State}.

handle_continue(Continue, State) ->
    case Continue of
        {setup, {Host, Port, DataBin}} ->
            try
                {ok, Socket} = gen_tcp:connect(Host, Port, [binary, {packet, 0}, {active, true}]),
                case DataBin of
                    <<>> ->
                        ignore;
                    _ ->
                        %io:format("send to ~p: ~p~n", [Socket, DataBin]),
                        ok = gen_tcp:send(Socket, DataBin)
                end,
                {noreply, State#state{socket=Socket}}
            catch
                _Type:Error ->
                    io:format("HTTP ~s:~p ~p~n", [Host, Port, Error]),
                    jpass_util:send_res(State#state.addr_info, stop, undefined),
                    ok = gen_server:stop(erlang:self()),
                    {noreply, State}
            end;
        _ ->
            io:format("jpass_server_http_conn:handle_continue() continue=~p, state=~p~n", [Continue, State]),
            {noreply, State}
    end.
    
code_change(_OldVsn, State, _Extra) -> 
    %io:format("jpass_server_http_conn:code_change() oldVsn=~p, state=~p, extra=~p~n", [_OldVsn, State, _Extra]),
    {ok, State}.

format_status(_Opt, [_PDict, _State]) ->
    %io:format("jpass_server_http_conn:format_status() opt=~p, pdict=~p, state=~p~n", [_Opt, _PDict, _State]),
    my_status_not_implemented.

terminate(Reason, _State) ->
    io:format("jpass_server_http_conn:terminate() with reason: ~p~n", [Reason]),
    ok.
