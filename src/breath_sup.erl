%% @author Bob Ippolito <bob@mochimedia.com>
%% @copyright 2007 Mochi Media, Inc.

-module (breath_sup).
-author ("Dong Yi <juvenpp@gmail.com>").

-behaviour (supervisor).


%% External exports
-export ([start_link/0, upgrade/0]).



%% supervisor callbacks
-export([init/1]).

%% @spec start_link() -> ServerRet
%% @doc API for starting the supervisor.
start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%% @spec upgrade() -> ok
%% @doc Add processes if necessary.
upgrade() ->
    mochisup:upgrade(?MODULE).

%% @spec init([]) -> SupervisorTree
%%      returns the supervisor tree.
init([]) ->
    SocketConfig = [{ip, 'localhost'},
		    {port, 8911}],
    Gateway = {breath_gateway,
             {breath_gateway, start, [SocketConfig]},
	         permanent, 5000, worker, dynamic},
    Proxy =  {breath_proxy,
             {breath_proxy, start_link, []},
             permanent, 5000, worker, [breath_proxy]},
    Scene =   {breath_scene,
             {breath_scene, start_link, []},
             permanent, 5000, worker, [breath_scene]},
    Util  =  {breath_util,
             {breath_util, start_link, []},
             permanent, 5000, worker, dynamic},
    Master = {breath_master,
            {breath_master, start_link, []},
            permanent, 5000, worker, [breath_master]},
    Stat = {breath_stat,
            {breath_stat, start_link, []},
            permanent, 5000, worker, [breath_stat]},

    Processes = [Gateway, Proxy, Proxy, Scene, Util, Master, Stat],
    {ok, {{one_for_one, 10, 10}, lists:flatten(Processes)}}.
