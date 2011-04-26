-module (breath_gateway).
-author ("Dong Yi <juvenpp@gmail.com>").

％ 全局入口，是个简单的socket server

-define (PORT, 54321).
-define (password, "d0ngy!").
-export ([start/0]).


start() ->
    case gen_tcp:listen(?PORT, [binary, {packet, 0}]) of
        {ok, Listen} -> 
            io:format("listen on ~p~n", [?PORT]),
            spawn(fun () -> parse_connect(Listen) end);
        {error, Reason} -> exit({?PORT, Reason})
    end.
    
parse_connect(Listen) ->
    case gen_tcp:accept(Listen) of 
        {ok, Socket} ->
            io:format("accept socket ~n"),
            loop(Socket);
        {error, Reason} -> exit({accept, Reason})
    end.
    
loop(Socket) ->
    receive
        {tcp, Socket, Data} ->
            io:format("coming pack ~p~n", [binary_to_list(Data)]),
            case rfc4627:decode(binary_to_list(Data)) of 
                {ok, D, _Remainder} ->
                    io:format("data:~p~n", [D]),
                    {obj, [{"password", Password}, {"body", Body}]} = D,
                    if
                        Password /= ?password ->
                            gen_tcp:send(Socket, <<"bad password">>),
                            loop(Socket)
                    end,
                    %交给下一步处理
                    Response = spawn_link(fun() -> respond(Socket) end),
                    breath_proxy:proxy(Body, Response),
                    loop(Socket); 
                {error, Reason} ->
                    io:format("bad ~p~n", [Reason]),
                    exit({?PORT, Reason})
            end;
        {tcp_closed, Socket} ->
            io:format("I'm closing ~n"),
            ok;
        _ ->
            io:format("i don't understand"),
            loop(Socket)
    end.

respond(Socket) ->
  receive
    {ok, Msg} -> gen_tcp:send(Socket, [ok, Msg])
  end.
