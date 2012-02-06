-module(breath_postoffice).
-behaviour(gen_server).

-export([start/1, stop/1]).
-export([init/1, handle_call/3, handle_cast/2, terminate/2, code_change/3,
         handle_info/2]).
-export([get/2, init/3]).

-record(breath_postoffice,
        {port,
         loop,
         name=undefined,
         ip=any,
         listen=null,
         nodelay=false,
         backlog=128,
         active_sockets=0,
         acceptor_pool_size=16,
         ssl=false,
         ssl_opts=[{ssl_imp, new}],
         acceptor_pool=sets:new()}).

%% We do not block on send anymore.
-define(TCP_SEND_TIMEOUT, 15000).

start(State=#breath_postoffice{}) ->
    start_server(State);
start(Options) ->
    start(parse_options(Options)).

get(Name, Property) ->
    gen_server:call(Name, {get, Property}).

stop(Name) when is_atom(Name) ->
    gen_server:cast(Name, stop);
stop(Pid) when is_pid(Pid) ->
    gen_server:cast(Pid, stop);
stop({local, Name}) ->
    stop(Name);
stop({global, Name}) ->
    stop(Name);
stop(Options) ->
    State = parse_options(Options),
    stop(State#breath_postoffice.name).

%% Internal API

parse_options(Options) ->
    parse_options(Options, #breath_postoffice{}).

parse_options([], State) ->
    State;
parse_options([{name, L} | Rest], State) when is_list(L) ->
    Name = {local, list_to_atom(L)},
    parse_options(Rest, State#breath_postoffice{name=Name});
parse_options([{name, A} | Rest], State) when A =:= undefined ->
    parse_options(Rest, State#breath_postoffice{name=A});
parse_options([{name, A} | Rest], State) when is_atom(A) ->
    Name = {local, A},
    parse_options(Rest, State#breath_postoffice{name=Name});
parse_options([{name, Name} | Rest], State) ->
    parse_options(Rest, State#breath_postoffice{name=Name});
parse_options([{port, L} | Rest], State) when is_list(L) ->
    Port = list_to_integer(L),
    parse_options(Rest, State#breath_postoffice{port=Port});
parse_options([{port, Port} | Rest], State) ->
    parse_options(Rest, State#breath_postoffice{port=Port});
parse_options([{ip, Ip} | Rest], State) ->
    ParsedIp = case Ip of
                   any ->
                       any;
                   Ip when is_tuple(Ip) ->
                       Ip;
                   Ip when is_list(Ip) ->
                       {ok, IpTuple} = inet_parse:address(Ip),
                       IpTuple
               end,
    parse_options(Rest, State#breath_postoffice{ip=ParsedIp});
parse_options([{loop, Loop} | Rest], State) ->
    parse_options(Rest, State#breath_postoffice{loop=Loop});
parse_options([{backlog, Backlog} | Rest], State) ->
    parse_options(Rest, State#breath_postoffice{backlog=Backlog});
parse_options([{nodelay, NoDelay} | Rest], State) ->
    parse_options(Rest, State#breath_postoffice{nodelay=NoDelay});
parse_options([{acceptor_pool_size, Max} | Rest], State) ->
    MaxInt = ensure_int(Max),
    parse_options(Rest,
                  State#breath_postoffice{acceptor_pool_size=MaxInt});
parse_options([{ssl, Ssl} | Rest], State) when is_boolean(Ssl) ->
    parse_options(Rest, State#breath_postoffice{ssl=Ssl});
parse_options([{ssl_opts, SslOpts} | Rest], State) when is_list(SslOpts) ->
    SslOpts1 = [{ssl_imp, new} | proplists:delete(ssl_imp, SslOpts)],
    parse_options(Rest, State#breath_postoffice{ssl_opts=SslOpts1}).


start_server(State=#breath_postoffice{ssl=Ssl, name=Name}) ->
    case Ssl of
        true ->
            ok = ensure_started(crypto),
            ok = ensure_started(public_key),
            ok = ensure_started(ssl);
        false ->
            ok
    end,
    case Name of
        undefined ->
            gen_server:start_link(?MODULE, State, []);
        _ ->
            gen_server:start_link(Name, ?MODULE, State, [])
    end.

%% @spec ensure_started(App::atom()) -> ok
%% @doc Start the given App if it has not been started already.
ensure_started(App) ->
    case application:start(App) of
        ok ->
            ok;
        {error, {already_started, App}} ->
            ok
    end.

ensure_int(N) when is_integer(N) ->
    N;
ensure_int(S) when is_list(S) ->
    list_to_integer(S).

ipv6_supported() ->
    case (catch inet:getaddr("localhost", inet6)) of
        {ok, _Addr} ->
            true;
        {error, _} ->
            false
    end.

init(State=#breath_postoffice{ip=Ip, port=Port, backlog=Backlog, nodelay=NoDelay}) ->
    process_flag(trap_exit, true),
    BaseOpts = [binary,
                {reuseaddr, true},
                {packet, 4},
                {backlog, Backlog},
                {send_timeout, ?TCP_SEND_TIMEOUT},
			    {keepalive, true},
                {active, false},
                {nodelay, NoDelay}],
    Opts = case Ip of
        any ->
            case ipv6_supported() of % IPv4, and IPv6 if supported
                true -> [inet, inet6 | BaseOpts];
                _ -> BaseOpts
            end;
        {_, _, _, _} -> % IPv4
            [inet, {ip, Ip} | BaseOpts];
        {_, _, _, _, _, _, _, _} -> % IPv6
            [inet6, {ip, Ip} | BaseOpts]
    end,
    listen(Port, Opts, State).

new_acceptor_pool(Listen,
                  State=#breath_postoffice{acceptor_pool=Pool,
                                                acceptor_pool_size=Size,
                                                loop=Loop}) ->
    F = fun (_, S) ->
                Pid = start_acceptor(self(), Listen, Loop),
                sets:add_element(Pid, S)
        end,
    Pool1 = lists:foldl(F, Pool, lists:seq(1, Size)),
    State#breath_postoffice{acceptor_pool=Pool1}.

listen(Port, Opts, State=#breath_postoffice{ssl=Ssl, ssl_opts=SslOpts}) ->
    case breath_gateway:listen(Ssl, Port, Opts, SslOpts) of
        {ok, Listen} ->
            {ok, ListenPort} = breath_gateway:port(Listen),
            {ok, new_acceptor_pool(
                   Listen,
                   State#breath_postoffice{listen=Listen,
                                                port=ListenPort})};
        {error, Reason} ->
            {stop, Reason}
    end.

do_get(port, #breath_postoffice{port=Port}) ->
    Port;
do_get(active_sockets, #breath_postoffice{active_sockets=ActiveSockets}) ->
    ActiveSockets.

handle_call({get, Property}, _From, State) ->
    Res = do_get(Property, State),
    {reply, Res, State};
handle_call(_Message, _From, State) ->
    Res = error,
    {reply, Res, State}.

handle_cast(stop, State) ->
    {stop, normal, State}.

terminate(_Reason, #breath_postoffice{listen=Listen}) ->
    breath_gateway:close(Listen).

code_change(_OldVsn, State, _Extra) ->
    State.

recycle_acceptor(Pid, State=#breath_postoffice{
                        acceptor_pool=Pool,
                        listen=Listen,
                        loop=Loop,
                        active_sockets=ActiveSockets}) ->
    case sets:is_element(Pid, Pool) of
        true ->
            Acceptor = start_acceptor(self(), Listen, Loop),
            Pool1 = sets:add_element(Acceptor, sets:del_element(Pid, Pool)),
            State#breath_postoffice{acceptor_pool=Pool1};
        false ->
            State#breath_postoffice{active_sockets=ActiveSockets - 1}
    end.

handle_info({'EXIT', Pid, normal}, State) ->
    {noreply, recycle_acceptor(Pid, State)};
handle_info({'EXIT', Pid, Reason},
            State=#breath_postoffice{acceptor_pool=Pool}) ->
    case sets:is_element(Pid, Pool) of
        true ->
            %% If there was an unexpected error accepting, log and sleep.
            error_logger:error_report({?MODULE, ?LINE,
                                       {acceptor_error, Reason}}),
            timer:sleep(100);
        false ->
            ok
    end,
    {noreply, recycle_acceptor(Pid, State)};

% this is what release_handler needs to get a list of modules,
% since our supervisor modules list is set to 'dynamic'
% see sasl-2.1.9.2/src/release_handler_1.erl get_dynamic_mods
handle_info({From, Tag, get_modules}, State = #breath_postoffice{name={local,Mod}}) ->
    From ! {element(2,Tag), [Mod]},
    {noreply, State};

% If for some reason we can't get the module name, send empty list to avoid release_handler timeout:
handle_info({From, Tag, get_modules}, State) ->
    error_logger:info_msg("breath_postoffice replying to dynamic modules request as '[]'~n",[]),
    From ! {element(2,Tag), []},
    {noreply, State};

handle_info(Info, State) ->
    error_logger:info_report([{'INFO', Info}, {'State', State}]),
    {noreply, State}.

start_acceptor(Server, Listen, Loop) ->
    proc_lib:spawn_link(?MODULE, init, [Server, Listen, Loop]).

init(Server, Listen, Loop) ->
    case catch breath_gateway:accept(Listen) of
        {ok, Socket} ->
            call_loop(Loop, Socket);
        {error, closed} ->
            exit(normal);
        {error, timeout} ->
            init(Server, Listen, Loop);
        {error, esslaccept} ->
            exit(normal);
        Other ->
            error_logger:error_report(
              [{application, breath_app},
               "Accept failed error",
               lists:flatten(io_lib:format("~p", [Other]))]),
            exit({error, accept_failed})
    end.

call_loop({M, F}, Socket) ->
    M:F(Socket);
call_loop({M, F, A}, Socket) ->
    erlang:apply(M, F, [Socket | A]);
call_loop(Loop, Socket) ->
    Loop(Socket).