-module(jpass_dns_sup).

-behaviour(supervisor).

-export([start_link/0, start_child/3]).
-export([init/1]).

-define(SERVER, ?MODULE).

start_link() ->
    %io:format("jpass_dns_sup:start_link()~n"),
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

init([]) ->
    %io:format("jpass_dns_sup:init()~n"),
    SupFlags = #{strategy => simple_one_for_one,
                 intensity => 2,
                 period => 10000},
    ChildSpecs = [#{id => jpass_dns_conn,
		    start => {jpass_dns_conn, start_link, []},
		    restart => temporary,
		    shutdown => brutal_kill,
		    type => worker,
		    modules => [jpass_dns_conn]}],
    {ok, {SupFlags, ChildSpecs}}.

start_child(FromIp, FromPort, Packet) ->
    supervisor:start_child(?SERVER, [FromIp, FromPort, Packet]).

