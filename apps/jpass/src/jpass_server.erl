-module(jpass_server).

-behaviour(gen_server).

-export([start_link/0]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, handle_continue/2, code_change/3, format_status/2, terminate/2]).

-record(state, {}).

-define(SERVER, ?MODULE).

start_link() ->
    %io:format("jpass_server:start_link()~n"),
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

init([]) ->
    %io:format("jpass_server:init()~n"),
    {ok, #state{}}.

handle_call(Request, From, State) ->
    io:format("jpass_server:handle_call() request=~p, from=~p, state=~p~n", [Request, From, State]),
    {reply, my_reply, State}.
    
handle_cast(Request, State) ->
    case Request of
        {{FromPid, RepeaterPid}, Cmd, CipherParams} ->
            Params = jpass_util:decode(CipherParams),
	    AddrInfo = {FromPid, RepeaterPid},
            case Cmd of
		tran_send ->
		    {ToIp, ToPort, DataBin} = Params,
		    io:format("~s TRAN ~pB TO ~p:~p ~n", [jpass_util:get_datetime_str(), size(DataBin), ToIp, ToPort]),
		    % use FromPid as ChildName
		    ChildName = erlang:list_to_atom(erlang:pid_to_list(FromPid)),
		    ChildCreated = lists:member(ChildName, erlang:registered()),
		    case ChildCreated of
			false -> 
			    jpass_server_tran_sup:start_child(ChildName, {AddrInfo, Params});
			true ->
			    ChildName ! {send, DataBin}
		    end;
		http_init ->
		    {ToIp, ToPort, DataBin} = Params,
		    io:format("~s HTTP INIT ~pB TO ~s:~p~n", [jpass_util:get_datetime_str(), size(DataBin), ToIp, ToPort]),
		    % use FromPid as ChildName
		    ChildName = erlang:list_to_atom(erlang:pid_to_list(FromPid)),
		    ChildCreated = lists:member(ChildName, erlang:registered()),
		    case ChildCreated of
			false -> 
			    AddrInfo = {FromPid, RepeaterPid},
			    jpass_server_http_sup:start_child(ChildName, {AddrInfo, Params});
			true ->
			    ChildName ! {send, DataBin}
		    end;
		http_send ->
		    DataBin = Params,
		    %io:format("~s HTTP SEND ~pB~n", [jpass_util:get_datetime_str(), size(DataBin)]),
		    ChildName = erlang:list_to_atom(erlang:pid_to_list(FromPid)),
		    ChildName ! {send, DataBin};
		dns_send ->
		    Packet = Params,
		    io:format("~s DNS ~pB~n", [jpass_util:get_datetime_str(), size(Packet)]),
		    jpass_server_dns_sup:start_child({AddrInfo, Packet});
		_ ->
		    io:format("jpass_server:handle_cast() request=~p, state=~p~n", [Request, State])
	    end;
	_ ->
	    io:format("jpass_server:handle_cast() request=~p, state=~p~n", [Request, State])
    end,
    {noreply, State}.

handle_info(Info, State) ->
   io:format("jpass_server:handle_info() info=~p, state=~p~n", [Info, State]),
   {noreply, State}.

handle_continue(_Continue, State) ->
    io:format("jpass_server:handle_continue() continue=~p, state=~p~n", [_Continue, State]),
    {noreply, State}.
    
code_change(_OldVsn, State, _Extra) -> 
    %io:format("jpass_server:code_change() oldVsn=~p, state=~p, extra=~p~n", [_OldVsn, _State, _Extra]),
    {ok, State}.

format_status(_Opt, [_PDict, _State]) ->
    %io:format("jpass_server:format_status() opt=~p, pdict=~p, state=~p~n", [_Opt, _PDict, _State]),
    my_status_not_implemented.

terminate(Reason, _State) ->
    io:format("jpass_server:terminate() with reason: ~p~n", [Reason]),
    ok.

