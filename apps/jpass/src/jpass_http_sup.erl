-module(jpass_http_sup).

-behaviour(supervisor).

-export([start_link/0, start_child/1]).
-export([init/1]).

-define(SERVER, ?MODULE).

start_link() ->
    %io:format("jpass_http_sup:start_link()~n"),
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

init([]) ->
    %io:format("jpass_http_sup:init()~n"),
    
    SupFlags = #{strategy => simple_one_for_one,
                 intensity => 2,
                 period => 10000},
    ChildSpecs = [#{id => jpass_http_conn,
		    start => {jpass_http_conn, start_link, []},
		    restart => temporary,
		    shutdown => brutal_kill,
		    type => worker,
		    modules => [jpass_http_conn]}],
    {ok, {SupFlags, ChildSpecs}}.

start_child(Socket) ->
    supervisor:start_child(?SERVER, [Socket]).
