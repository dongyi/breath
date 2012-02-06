-module(breath_sup).

-behaviour(supervisor).

%% External exports
-export([start_link/0]).

%% supervisor callbacks
-export([init/1]).

%% @spec start_link() -> ServerRet
%% @doc API for starting the supervisor.
start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%% @spec init([]) -> SupervisorTree
%% @doc supervisor callback.
init([]) ->
    Controller = controller_specs(),

    BreathConfig = [{ip, "0.0.0.0"}, {port, 2223}],
    Breath = {breath_server,
           {breath_mod, start, [BreathConfig]},
            permanent, 5000, worker, dynamic},
    Processes = [Breath, Controller],
    io:format("Processes:~p~n", [Processes]),
    {ok, {{one_for_one, 10, 10}, lists:flatten(Processes)}}.

controller_specs() ->
    child_spec(breath_proxy).
     
child_spec(Module) ->
    Module:child_spec().
