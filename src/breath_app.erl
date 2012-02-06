-module(breath_app).
-behaviour(application).

-export([start/2,stop/1]).

%% @spec start(_Type, _StartArgs) -> ServerRet
%% @doc application start callback for breath server.
start(_Type, _StartArgs) ->
    breath_sup:start_link().

%% @spec stop(_State) -> ServerRet
%% @doc application stop callback for breath server.
stop(_State) ->
    ok.