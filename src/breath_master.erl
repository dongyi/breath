-module (breath_master).
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

-record (state, {scenelist=[]}).

% 记录场景对应表
-record (scenemap, {sceneid, scenepid, current_number}).

start_link() -> gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

init(_Args) ->
         process_flag(trap_exit, true),
         {ok, SceneMap} = dets:open_file("scene.dets", []),
         _SceneTable = ets:new(scenemap, [public,set,named_table,{keypos, #scenemap.sceneid}]),
         {ok, #state{scenelist=SceneMap}}.

login(Userid, Sceneid) -> gen_server:call(?MODULE, {login, {Userid, Sceneid}}, infinity).
logout(Userid, Sceneid) -> gen_server:call(?MODULE, {logout, {Userid, Sceneid}}.


handle_call(login, {Userid, Sceneid}) ->
    % 如果有这个场景id 就登录  否则创建一个场景进程
    % 本模块内保存了一个所有场景id- 场景pid的关系
    % 返回场景pid , 和场景的人数
    Result = ets:lookup(scenemap, Sceneid),
    case Result of
      [] ->
        ets:insert(scenemap, []),
        ScenePid = breath_scene:join(Userid, Sceneid),
        {noreply, {ok, ScenePid, 1}};
      [Sceneid, Scenepid, Current_number] ->
        {reply, {ok, Scenepid, Current_number}, State}
    end.


handle_call(logout, {Userid, Sceneid}) ->
  {reply, {ok}, State}.
