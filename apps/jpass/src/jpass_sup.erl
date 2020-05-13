-module(jpass_sup).

-behaviour(supervisor).

-export([start_link/0]).
-export([init/1]).

-define(SERVER, ?MODULE).

start_link() ->
    %io:format("jpass_sup:start_link()~n"),
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

init([]) ->
    %io:format("jpass_sup:init()~n"),

    SupFlags = #{strategy => one_for_all,
                 intensity => 5,
                 period => 10000},

    HttpChildSpecs = [#{id => jpass_http_listener,
		        start => {jpass_http_listener, start_link, []},
		        restart => permanent,
		        shutdown => brutal_kill,
		        type => worker,
		        modules => [jpass_http_listener]},
                      #{id => jpass_http_sup,
		        start => {jpass_http_sup, start_link, []},
		        restart => permanent,
		        shutdown => brutal_kill,
		        type => supervisor,
		        modules => [jpass_http_sup]}
    ],
    TranChildSpecs = [#{id => jpass_tran_listener,
		        start => {jpass_tran_listener, start_link, []},
		        restart => permanent,
		        shutdown => brutal_kill,
		        type => worker,
		        modules => [jpass_tran_listener]},
                      #{id => jpass_tran_sup,
		        start => {jpass_tran_sup, start_link, []},
		        restart => permanent,
		        shutdown => brutal_kill,
		        type => supervisor,
		        modules => [jpass_tran_sup]}
    ],
    DnsChildSpecs = [#{id => jpass_dns_listener,
		    start => {jpass_dns_listener, start_link, []},
		    restart => permanent,
		    shutdown => brutal_kill,
		    type => worker,
		    modules => [jpass_dns_listener]},
                  #{id => jpass_dns_sup,
		    start => {jpass_dns_sup, start_link, []},
		    restart => permanent,
		    shutdown => brutal_kill,
		    type => supervisor,
		    modules => [jpass_dns_sup]}
    ],
    ServerChildSpecs = [#{id => jpass_server,
		          start => {jpass_server, start_link, []},
		          restart => permanent,
		          shutdown => brutal_kill,
		          type => worker,
		          modules => [jpass_server]},
                        #{id => jpass_server_tran_sup,
		          start => {jpass_server_tran_sup, start_link, []},
		          restart => permanent,
		          shutdown => brutal_kill,
		          type => supervisor,
		          modules => [jpass_server_tran_sup]},
                        #{id => jpass_server_http_sup,
		          start => {jpass_server_http_sup, start_link, []},
		          restart => permanent,
		          shutdown => brutal_kill,
		          type => supervisor,
		          modules => [jpass_server_http_sup]},
                        #{id => jpass_server_dns_sup,
		          start => {jpass_server_dns_sup, start_link, []},
		          restart => permanent,
		          shutdown => brutal_kill,
		          type => supervisor,
		          modules => [jpass_server_dns_sup]}
    ],

    RepeaterChildSpecs = [#{id => jpass_repeater,
		    start => {jpass_repeater, start_link, []},
		    restart => permanent,
		    shutdown => brutal_kill,
		    type => worker,
		    modules => [jpass_repeater]}
    ],

    {ok, Http} = application:get_env(http_enabled),
    {ok, Tran} = application:get_env(tran_enabled),
    {ok, Dns} = application:get_env(dns_enabled),
    {ok, Server} = application:get_env(server_enabled),
    {ok, Repeater} = application:get_env(repeater_enabled),
    io:format("HTTP=~s, TRAN=~s, DNS=~s, SERVER=~s, REPEATER=~s~n", [Http, Tran, Dns, Server, Repeater]),
    
    ChildSpecs = case Http of true -> HttpChildSpecs; _ -> [] end ++ 
                 case Tran of true -> TranChildSpecs; _ -> [] end ++ 
                 case Dns of true -> DnsChildSpecs; _ -> [] end ++ 
                 case Server of true -> ServerChildSpecs; _ -> [] end ++ 
                 case Repeater of true -> RepeaterChildSpecs; _ -> [] end,
        
    {ok, {SupFlags, ChildSpecs}}.
