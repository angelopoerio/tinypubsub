%% Author: Angelo Poerio <angelo.poerio@gmail.com>

-module(pub_sub_sup).
-behaviour(supervisor).
-export([start_link/0, init/1]).

start_link() ->
    supervisor:start_link({global,?MODULE}, ?MODULE, []).

init([]) ->
	 	Controller_proc = {controller, {controller, start_link, []}, 
	 		permanent, 2000, worker, [controller]},
        Tcp_server_proc = {pub_sub, {pub_sub, start, [4444]},
           permanent, 2000, worker, [pub_sub]},
  		{ok, {{one_for_one, 1, 1}, [Controller_proc, Tcp_server_proc]}}.

