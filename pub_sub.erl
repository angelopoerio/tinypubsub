%% Author: Angelo Poerio <angelo.poerio@gmail.com>

-module(pub_sub).

-export([start/1, loop/1]).

start(Port) ->
    controller:start_link(),
    tcp_server:start(?MODULE, Port, {?MODULE, loop}).

loop(Socket) ->
    case gen_tcp:recv(Socket, 0) of
        {ok, Data} ->
            Message = binary_to_list(Data),
            {Command, [_|Command_arg]} = lists:splitwith(fun(T) -> [T] =/= " " end, clean(Message)),
            case Command of
                "PUB" -> {Key, [_|Value]} = lists:splitwith(fun(T) -> [T] =/= " " end, Command_arg),
                         publish_variable(Key, Value),
                         loop(Socket);
                "SUB" -> subscribe_variable(Socket, Command_arg), 
                         loop(Socket);
                "UNSUB" -> unsubscribe_variable(Socket, Command_arg), 
                           loop(Socket);
                _ ->
                         gen_tcp:send(Socket, "Unknown command!\n"),
                         loop(Socket)
            end;
        {error, closed} -> ok
    end.

publish_variable(Key, Value) -> controller:publish(Key, Value).

subscribe_variable(Socket, Key) -> controller:subscribe(Socket, Key).

unsubscribe_variable(Socket, Key) -> controller:unsubscribe(Socket, Key).

clean(Data) -> string:strip(Data, both, $\n).