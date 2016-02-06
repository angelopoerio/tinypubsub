%% Author: Angelo Poerio <angelo.poerio@gmail.com>

-module(pub_sub_app).
-behaviour(application).
-export([start/2, stop/1]).
 
start(normal, _Args) -> pub_sub_sup:start_link().
 
stop(_State) -> ok.