-module(breath_gateway).

-export([listen/4, accept/1, recv/3, send/2, close/1, port/1, peername/1,
         setopts/2]).

-define(ACCEPT_TIMEOUT, 2000).

listen(Ssl, Port, Opts, SslOpts) ->
    case Ssl of
        true ->
            case ssl:listen(Port, Opts ++ SslOpts) of
                {ok, ListenSocket} ->
                    {ok, {ssl, ListenSocket}};
                {error, _} = Err ->
                    Err
            end;
        false ->
            gen_tcp:listen(Port, Opts)
    end.

accept({ssl, ListenSocket}) ->
    try ssl:transport_accept(ListenSocket) of
        {ok, Socket} ->
            case ssl:ssl_accept(Socket) of
                ok ->
                    {ok, {ssl, Socket}};
                {error, _} = Err ->
                    Err
            end;
        {error, _} = Err ->
            Err
    catch
        error:{badmatch, {error, Reason}} ->
            {error, Reason}
    end;
accept(ListenSocket) ->
    gen_tcp:accept(ListenSocket, ?ACCEPT_TIMEOUT).

recv({ssl, Socket}, Length, Timeout) ->
    ssl:recv(Socket, Length, Timeout);
recv(Socket, Length, Timeout) ->
    gen_tcp:recv(Socket, Length, Timeout).

send({ssl, Socket}, Data) ->
    ssl:send(Socket, Data);
send(Socket, Data) ->
    gen_tcp:send(Socket, Data).

close({ssl, Socket}) ->
    ssl:close(Socket);
close(Socket) ->
    gen_tcp:close(Socket).

port({ssl, Socket}) ->
    case ssl:sockname(Socket) of
        {ok, {_, Port}} ->
            {ok, Port};
        {error, _} = Err ->
            Err
    end;
port(Socket) ->
    inet:port(Socket).

peername({ssl, Socket}) ->
    ssl:peername(Socket);
peername(Socket) ->
    inet:peername(Socket).

setopts({ssl, Socket}, Opts) ->
    ssl:setopts(Socket, Opts);
setopts(Socket, Opts) ->
    inet:setopts(Socket, Opts).