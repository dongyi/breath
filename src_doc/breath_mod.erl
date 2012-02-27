-module(breath_mod).

-export([start/0, start/1, stop/0, stop/1]).
-export([loop/1]).

-define(DEFAULTS, [{name, ?MODULE},
                   {ip, "127.0.0.1"},
                   {port, 6666}]).

parse_options(Options) ->
    Loop = fun (S) ->
                   ?MODULE:loop(S)
           end,
    Options1 = [{name, ?MODULE}, {loop, Loop} | proplists:delete(loop, Options)],
    set_defaults(?DEFAULTS, Options1).

%% @spec set_default({Key::term(), Value::term()}, Proplist::list()) -> list()
%%
%% @doc Return new Proplist with {Key, Value} set if not is_defined(Key, Proplist).
set_default({Key, Value}, Proplist) ->
    case is_defined(Key, Proplist) of
        true ->
            Proplist;
        false ->
            [{Key, Value} | Proplist]
    end.

%% @spec set_defaults([{Key::term(), Value::term()}], Proplist::list()) -> list()
%%
%% @doc Return new Proplist with {Key, Value} set if not is_defined(Key, Proplist).
set_defaults(DefaultProps, Proplist) ->
    lists:foldl(fun set_default/2, Proplist, DefaultProps).

%% @spec is_defined(Key::term(), Proplist::list()) -> bool()
%%
%% @doc Returns true if Propist contains at least one entry associated
%%      with Key, otherwise false is returned.
is_defined(Key, Proplist) ->
    lists:keyfind(Key, 1, Proplist) =/= false.

stop() ->
    breath_postoffice:stop(?MODULE).

stop(Name) ->
    breath_postoffice:stop(Name).

start() ->
    start(?DEFAULTS).

%% @spec start(Options) -> ServerRet
%%     Options = [option()]
%%     Option = {name, atom()} | {ip, string() | tuple()} | {backlog, integer()}
%%              | {nodelay, boolean()} | {acceptor_pool_size, integer()}
%%              | {ssl, boolean()}
%% @doc Start a server.
%%      After each accept, if defined, profile_fun is called with a proplist of a subset of the breath_postoffice state and timing information.
%%      The proplist is as follows: [{name, Name}, {port, Port}, {active_sockets, ActiveSockets}].
%% @end
start(Options) ->
    breath_postoffice:start(parse_options(Options)).

loop(Socket) ->
    loop(Socket, breath_proxy).

loop(Socket, Name) ->
    ok = breath_gateway:setopts(Socket, [{active, true}]),
    receive
        {setController, Pid} ->
             loop(Socket, Pid);
        {Protocol, Socket, Bin} when Protocol == tcp; Protocol == ssl ->
             Msg = binary_to_term(Bin),
             io:format("Msg:~p~n", [Msg]),
             ok = gen_server:call(Name, {self(), Msg}),
             loop(Socket, Name);
        {send, Term} ->
             breath_gateway:send(Socket, term_to_binary(Term)),
             loop(Socket, Name);
        {Type, _Socket} when Type == tcp_closed; Type == ssl ->
             gen_server:cast(Name, {Type, self()}),
             exit(normal);
        _Other ->
             breath_gateway:close(Socket),
             exit(normal)
     end.