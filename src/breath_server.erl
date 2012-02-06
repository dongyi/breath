-module(breath_server).

-export([start/0, stop/0]).

start() ->
    prepare(),
    application:start(breath_server).

stop() ->
    application:stop(breath_server).

prepare() ->
    breath_deps:ensure(),
    ensure_started(crypto),
    ensure_started(public_key),
    ensure_started(inets),
    ensure_started(ssl).

ensure_started(App) ->
    case application:start(App) of
        ok ->
            ok;
        {error, {already_started, App}} ->
            ok
    end.