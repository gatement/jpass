-module(jpass_http_conn).

-behaviour(gen_server).

-export([start_link/1, hand_off/1]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, handle_continue/2, code_change/3, format_status/2, terminate/2]).

-record(state, {socket = undefined, transmit = false, data_bin = <<>>}).

-define(SERVER, ?MODULE).

start_link(Socket) ->
    %io:format("jpass_http_conn:start_link()~n"),
    gen_server:start_link(?MODULE, [Socket], []).

init([Socket]) ->
    %io:format("jpass_http_conn:init() socket=~p~n", [Socket]),
    {ok, #state{socket=Socket}}.

handle_call(Request, From, State) ->
    io:format("jpass_http_conn:handle_call() request=~p, from=~p, state=~p~n", [Request, From, State]),
    {reply, my_reply, State}.
    
handle_cast(Request, State) ->
    Socket = State#state.socket,
    case Request of
        hand_off ->
            ok = inet:setopts(Socket, [{active, true}]);
        _ ->
	    %io:format("jpass_http_conn:handle_cast() request=~p, state=~p~n", [Request, State]),
            ignore
    end,
    {noreply, State}.

handle_info(Info, State) ->
    %io:format("~p jpass_http_conn:handle_info() info=~p, state=~p~n", [erlang:self(), Info, State]),
    Socket = State#state.socket,
    case Info of
        {tcp, Socket, DataBin} ->
            %io:format("~p ~p~n", [erlang:self(), {tcp, Socket, DataBin}]),
	    State2 = process_client_data(State, DataBin),
	    {noreply, State2};
        {tcp_closed, _Socket} ->
            %io:format("~p ~p~n", [erlang:self(), {tcp_closed, _Socket}]),
            ok = gen_tcp:close(Socket),
            ok = gen_server:stop(erlang:self()),
            {noreply, State};
        {_AddrInfo, recv, CipherParams} ->
            DataBin = jpass_util:decode(CipherParams),
            %io:format("~s ~p HTTP RECV ~pB~n", [jpass_util:get_datetime_str(), erlang:self(), size(DataBin)]),
            gen_tcp:send(Socket, DataBin),
            {noreply, State};
        {_AddrInfo, stop, _Params} ->
            %io:format("~p ~p~n", [erlang:self(), {_AddrInfo, stop, _Params}]),
            ok = gen_tcp:close(Socket),
            ok = gen_server:stop(erlang:self()),
            {noreply, State};
        _ ->
            io:format("jpass_http_conn:handle_info() info=~p, state=~p~n", [Info, State]),
            {noreply, State}
    end.

handle_continue(_Continue, State) ->
    io:format("jpass_http_conn:handle_continue() continue=~p, state=~p~n", [_Continue, State]),
    {noreply, State}.
    
code_change(_OldVsn, State, _Extra) -> 
    %io:format("jpass_http_conn:code_change() oldVsn=~p, state=~p, extra=~p~n", [_OldVsn, State, _Extra]),
    {ok, State}.

format_status(_Opt, [_PDict, _State]) ->
    %io:format("jpass_http_conn:format_status() opt=~p, pdict=~p, state=~p~n", [_Opt, _PDict, _State]),
    my_status_not_implemented.

terminate(Reason, _State) ->
    io:format("jpass_http_conn:terminate() with reason: ~p~n", [Reason]),
    ok.

hand_off(ChildPid) ->
    gen_server:cast(ChildPid, hand_off).

process_client_data(State, DataBin) ->
    Transmit = State#state.transmit,
    case Transmit of
	false ->
	    init_conn(State, DataBin);
	_ ->
            %io:format("~s ~p HTTP SEND ~pB~n", [jpass_util:get_datetime_str(), erlang:self(), size(DataBin)]),
            jpass_util:send_req(http_send, DataBin),
            State
    end.
     
init_conn(State, DataBin) ->
    %io:format("~p jpass_http_conn:init_conn() dataBin: ~p~n", [erlang:self(), DataBin]),
    case string:find(DataBin, "\r\n\r\n") of
        nomatch ->
            %% continue receiving
            DataBin2 = <<(State#state.data_bin)/binary, DataBin/binary>>,
            State#state{data_bin = DataBin2};
        _Body ->
            FirstLine = get_first_line(DataBin),
            {Method, Url} = get_method_url(FirstLine),
            {Host, Port} = get_host_port(Method, Url),
	    %io:format("jpass_http_conn:init_conn() connect to ~s:~p~n", [Host, Port]),
	    io:format("~s ~p HTTP ~s ~s~n", [jpass_util:get_datetime_str(), erlang:self(), erlang:binary_to_list(Method), Url]),
            
            case Method of
	        <<"CONNECT">> ->
                    %% send CONNECT response
                    ConnectResponse = <<"HTTP/1.1 200 Connection Established\r\n\r\n">>,
                    Socket = State#state.socket,
                    ok = gen_tcp:send(Socket, ConnectResponse),
                    %io:format("~p send CONNECT resp: ~p~n", [erlang:self(), ConnectResponse]),
		    jpass_util:send_req(http_init, {Host, Port, <<>>});
                _ ->
                    %io:format("~s ~p HTTP SEND ~pB~n", [jpass_util:get_datetime_str(), erlang:self(), size(DataBin)]),
		    jpass_util:send_req(http_init, {Host, Port, DataBin})
            end,

            State#state{transmit=true}
    end.

get_first_line(DataBin) ->
    Headers = string:lexemes(DataBin, [[$\r,$\n]]),
    [FirstLine | _Rest ] = Headers,
    FirstLine.

get_method_url(FirstLine) ->
    %% first line example1: CONNECT www.example.com:443 HTTP/1.1
    %% first line example2: POST http://www.example.com/a/b/c HTTP/1.1
    
    Words = string:lexemes(FirstLine, " "),
    [Method, Url | _Rest] = Words,
    {Method, Url}.

get_host_port(Method, Url) ->
    case Method of
	<<"CONNECT">> ->
	    [HostBin, PortBin] = string:lexemes(Url, ":"),
	    {erlang:binary_to_list(HostBin), erlang:binary_to_integer(PortBin)};
	_ ->
	    [_Scheme, HostPort | _Rest] = string:lexemes(Url, "/"),
	    case string:lexemes(HostPort, ":") of
		[HostBin, PortBin] ->
	            {erlang:binary_to_list(HostBin), erlang:binary_to_integer(PortBin)};
		[HostBin] ->
	            {erlang:binary_to_list(HostBin), 80}
	    end
    end.
