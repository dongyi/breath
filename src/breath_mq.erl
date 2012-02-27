-module(breath_mq).
-behavior(gen_server).
-export([start_link/0]).
-export([child_spec/0]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
-export([send_msg/1]).

-record(state, {socket}).


%% @spec child_spec() -> child_spec()
%%
%% @doc
child_spec() ->
    {?MODULE, {?MODULE, start_link, []},
     permanent, 5000, worker, [?MODULE]}.

%% @spec start_link() -> {ok, state}
%%
%% @doc
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

send_msg(Msg) ->
    gen_server:cast(?MODULE, {send, Msg}).

init([]) ->
    process_flag(trap_exit, true),
   {ok, Context} = erlzmq:context(),
   {ok, Socket} = erlzmq:socket(Context, [pub, {active, false}]),
   ok = erlzmq:bind(Socket, "tcp://127.0.0.1:5858"),
   {ok, #state{socket = Socket}}.

terminate(_Reason, State) ->
    ok = erlzmq:close(State#state.socket).

handle_cast({send, Msg}, #state{socket=Socket}=State) ->
    io:format('test---in handle cast ~n'),
    erlzmq:send(Socket, Msg),
    {noreply, State}.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

handle_call(_Req, _From, State) ->
    {noreply, ok, State}.

handle_info(_Req, State) ->
    {noreply, State}.

