-module(test).
-export([test_all/0, loop/1]).


loop(Socket) ->
   {ok, Res} = erlzmq:recv(Socket),
   io:format("receive msg: ~p ~n", [Res]),
   loop(Socket).

test_all() ->
   code:add_path("../ebin"),
   code:add_path("../lib/erlzmq2/ebin"),
   {ok, Context} = erlzmq:context(),
   {ok, Socket} = erlzmq:socket(Context, [pull, {active, false}]),
   erlzmq:connect(Socket, "tcp://127.0.0.1:2224"),
   loop(Socket).

