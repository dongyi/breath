-module(breath_proxy).
-behaviour(gen_server).

-export([child_spec/0, start_link/0]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-export([get_state/0, get_group_users/1, get_all_groups_users/0]).

-record(state, {groups}).

%% @spec child_spec() -> child_spec()
%%
%% @doc
child_spec() ->
    {?MODULE, {?MODULE, start_link, []},
     permanent, 5000, worker, [?MODULE]}.

%% @spec start_link() -> {ok, pid()}
%%
%% @doc
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

%% @spec get_state() -> lists()
%% @doc
get_state() ->
    gen_server:call(?MODULE, get_state).

%% @spec get_group_users(string()) -> lists()
%% @doc
get_group_users(Group) ->
    #state{groups = Groups} = get_state(),
    Pid = orddict:fetch(Group, Groups),
    gen_server:call(Pid, get_all_users).

%% @spec get_all_groups_users() -> lists()
%% @doc
get_all_groups_users() ->
    #state{groups = Groups} = get_state(),
    [{G, gen_server:call(Pid, get_all_users)} || {G, Pid} <- Groups].

init([]) ->
    process_flag(trap_exit, true),
    {ok, #state{groups = orddict:new()}}.

handle_call(get_state, _From, State) ->
    {reply, State, State};
handle_call({Channel, {startService, chat, _ArgC}}, _From, State) ->
    Channel ! {send, ack},
    {reply, ok, State};
handle_call({_Channel, {create, Group}}, _From,
               #state{groups = Groups} = State) ->
    io:format("create new group: ~s'~n", [Group]),
    NewState = case orddict:is_key(Group, Groups) of
		           true -> State;
                   false ->
                       {ok, Pid} = breath_room:start_link(Group),
                       Groups0 = orddict:store(Group, Pid, Groups),
                       State#state{groups = Groups0}
	           end,
    {reply, ok, NewState};
handle_call({Channel, {login, Group, Nick}}, _From,
               #state{groups = Groups} = State) ->
    io:format("login group: ~p'~n", [Groups]),
    NewState = case orddict:is_key(Group, Groups) of
		           true ->
                       Channel ! {send, ack},
                       Pid = orddict:fetch(Group, Groups),
                       ok = gen_server:call(Pid, {login, Channel, Nick}),
                       State;
                   false ->
                       Channel ! {send, ack},
                       {ok, Pid} = breath_room:start_link(Group),
                       erlang:monitor(process, Pid),
                       ok = gen_server:call(Pid, {login, Channel, Nick}),
                       Groups0 = orddict:store(Group, Pid, Groups),
                       State#state{groups = Groups0}
	           end,
    {reply, ok, NewState};
handle_call(Req, _From, State) ->
    io:format("~p~n: error_req:~p~n", [?MODULE, Req]),
    {reply, {error, Req}, State}.

handle_cast(_Req, State) ->
    {noreply, State}.

handle_info({'DOWN', _MRef, process, Pid, _Info},
               #state{groups = Groups} = State) ->
    io:format("~p, ~p: Pid:~p~n", [?MODULE, ?LINE, Pid]),
    NewGroups = remove_group(Pid, Groups),
    {noreply, State#state{groups = NewGroups}};
handle_info(_Req, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

remove_group(Pid, [{G,Pid}|T]) -> io:format("~p removed~n",[G]), T;
remove_group(Pid, [H|T])       -> [H|remove_group(Pid, T)];
remove_group(_, [])            -> [].