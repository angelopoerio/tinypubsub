%% Author: Angelo Poerio <angelo.poerio@gmail.com>

-module(controller).
-behaviour(gen_server).

-export([publish/2, subscribe/2, unsubscribe/2, notify_change/4]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3, start_link/0]).

%% The state holds two dict:
%% the first one is to store the key/value pairs
%% the second one is to store the subscribers
init([]) -> {ok, [dict:new(), dict:new()]}.

%% start the process
start_link() -> gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

%% api
publish(Key, Value) -> gen_server:call(?MODULE, {publish, Key, Value}).

subscribe(Socket, Key) -> gen_server:call(?MODULE, {subscribe, Socket, Key}).

unsubscribe(Socket, Key) -> gen_server:call(?MODULE, {unsubscribe, Socket, Key}).

%% utility function to notify subscribers of the changed value
notify_change(K_V_dict, Pub_Sub_dict, Key, New_val) -> 
	case dict:is_key(Key, K_V_dict) of
		true -> Send_n = fun(Socket) -> gen_tcp:send(Socket, "CNGD " ++ Key ++ " " ++ New_val ++ " " ++ " \n") end,
				lists:map(Send_n, dict:fetch(Key, Pub_Sub_dict)),
				changed;
		_    -> not_changed
	end.

%% gen_server callbacks
handle_call({publish, Key, Value}, _From, D) -> 
	K_V_dict = lists:nth(1, D),
	Pub_Sub_dict = lists:nth(2, D),
	spawn(?MODULE, notify_change, [K_V_dict, Pub_Sub_dict, Key, Value]), %% create a new process to notify subscribers
	{reply, ok, [dict:store(Key, Value, K_V_dict), Pub_Sub_dict]};

handle_call({subscribe, Socket, Key}, _From, D) ->
	K_V_dict = lists:nth(1, D),
	Pub_Sub_dict = lists:nth(2, D),
	case dict:is_key(Key, K_V_dict) of
		true -> {reply, ok, [K_V_dict, dict:append(Key, Socket, Pub_Sub_dict)]};
		_ -> gen_tcp:send(Socket, "Can't subscribe to a not existing key\n"), {reply, ok, [K_V_dict, Pub_Sub_dict]}
	end;

handle_call({unsubscribe, Socket, Key}, _From, D) ->
	K_V_dict = lists:nth(1, D),
	Pub_Sub_dict = lists:nth(2, D),
	Subscribers = dict:fetch(Key, Pub_Sub_dict), 
    {reply, ok, [K_V_dict, dict:store(Key, lists:delete(Socket, Subscribers), Pub_Sub_dict)]}.

handle_cast(_Msg, N) ->
  {noreply, N}.
handle_info(_Msg, N) ->
  {noreply, N}.
code_change(_OldVsn, N, _Other) ->
  {ok, N}.
terminate(_Reason, _N) ->
  ok.
