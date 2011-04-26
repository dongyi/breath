-module (breath_scene).
-author ("Dong Yi <juvenpp@gmail.com").

-behaviour (gen_server).

-export ([start_link/0]).

-export ([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-export ([login/2, leave/2, broadcast/2, get_scene_list/1]).

-define (MAX_IDLE_TIME, 160).

-define (RATE_MSG_INTERVAL, 3).
-define (RATE_MSG_COUNT, 10).

-define (MAX_CONNECTIONS, 3).

-define (ADMIN_PASSWORD, "dongyi").

-record (state, {clients = []}).


% 这个进程是场景进程，每个进程保存一个在线uid的列表
% 对外提供join， leave， broadcast接口

start_link() -> gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).



breath_scene:broadcast(Userid, Scenepid, Msg) -> gen_server:call(?MODULE, {broadcast, {Scenepid, Msg}}, infinity).
breath_scene:leave(Userid, Scenepid, Msg) -> gen_server:call(?MODULE, {}).
breath_scene:join(Userid, Sceneid) -> gen_server:call(?MODULE, {})  %返回一个pid


handle_call({broadcast, {Scenepid, Msg}}) ->
    ok.
    
    
    