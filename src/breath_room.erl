-module(breath_room).

-behaviour(gen_server).

-export([start_link/1]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-record(state, {room, users}).

%% @spec start_link(Name) -> {ok, pid()}
%%
%% @doc
start_link(Name) ->
    gen_server:start_link(?MODULE, [Name], []).

init([Name]) ->
    {ok, #state{room = Name, users = orddict:new()}}.

handle_call(get_all_users, _From, #state{users = Users} = State) ->
    Users0 = [element(2, V) || V <- Users],
    {reply, Users0, State};
handle_call({login, Pid, Nick}, _From, #state{users = Users} = State) ->
    NewState = case orddict:is_key(Pid, Users) of
                   true ->
                       broadcast(Nick, Pid, "connected", Users),
                       Pid ! {setController, self()},
                       State;
                   false ->
                       Users0 = orddict:store(Pid, Nick, Users),
                       io:format("pid:~p~n", [Pid]),
                       broadcast(Nick, Pid, "connected", Users0),
                       Pid ! {setController, self()},
                       State#state{users = Users0}
               end,
    {reply, ok, NewState};
handle_call({Pid, {relay, Nick, Msg}}, _From, #state{users = Users} = State) ->
    broadcast(Nick, Pid, Msg, Users),
    {reply, ok, State};
handle_call(Req, _From, State) ->
    io:format("~p~n:Req:~p~n", [?MODULE, Req]),
    {reply, {error, Req}, State}.

handle_cast({_, Pid}, #state{users = Users} = State) ->
    Nick = orddict:fetch(Pid, Users),
    NewUsers = remove_user(Pid, Users),
    broadcast(Nick, Pid, "connection closed", NewUsers),
    {noreply, State#state{users = NewUsers}};
handle_cast(_Req, State) ->
    {noreply, State}.

handle_info(_Req, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

broadcast(_Nick, _Pid, _Msg, []) ->
    exit(normal);
broadcast(Nick, Pid, Msg, Users) ->
    case string:tokens(Msg, ":") of
        [To, Content] ->
            case is_in_list(string:strip(To), Users) of
                false ->
                    F = fun(User) ->
                        %%io:format("Broadcasting ~p to ~p", [Msg, User]),
                        send(Nick, Pid, Msg, User)
                    end,
                    lists:foreach(F, orddict:fetch_keys(Users));
                User ->
                    send(Nick, Pid, Content, User)
            end;
        [Content] ->
            case string:tokens(Content, "@") of
                [Mq_header, Mq_body] ->
                    breath_mq:send_msg(Mq_header++"@"++Mq_body);
                [Mq_body] ->
                    F = fun(User) ->
                        %%io:format("Broadcasting ~p to ~p", [Msg, User]),
                        send(Nick, Pid, Mq_body, User)
                    end,
                    lists:foreach(F, orddict:fetch_keys(Users))
            end
    end.

send(Nick, Pid, Msg, To) when is_pid(To); is_atom(To) ->
    To ! {send, {msg, Nick, Pid, Msg}}.

remove_user(_P, [{_P, _N} | _T]) -> _T;
remove_user(_P, [_H | _T])     -> [_H | remove_user(_P, _T)];
remove_user(_, [])        -> [].

is_in_list(_N, [{_P, _N} | _T]) -> _P;
is_in_list(_N, [{_P, _} | _T]) -> is_in_list(_N, _T);
is_in_list(_N, []) -> false.
