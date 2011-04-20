-module (breath_proxy).
-author ("Dong Yi <juvenpp@gmail.com>").

-behaviour(gen_server).

%% API
-export([start_link/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

-record(state, {}).

%%
%% 

% JSON Helpers
json_client_ok(Msg) -> lists:flatten(rfc4627:encode({obj, [{"status", <<"ok">>}, {response, Msg}]})).
json_client_error(Msg) -> lists:flatten(rfc4627:encode({obj, [{"status", <<"error">>}, {response, Msg}]})).
json_respond(Msg, Req) -> Req ! {response, Msg}.

format_data(admin_logged_out, Uid) -> Uid;
format_data(admin_logged_in, Uid) -> Uid;
format_data(user_left_room, {Uid, Reason}) -> {obj, [{"uid", Uid}, {"reason", Reason}]};
format_data(user_joined_room, Uid) -> Uid;
format_data(system_msg, Msg) -> Msg;
format_data(X, {Uid, Msg}) when (X == chat_msg) or (X == sent_chat_msg) -> {obj, [{"uid", Uid}, {"msg", Msg}]};
format_data(_, _) -> [].

format_message({Id, {Type, Data}}) -> {obj, [{"id", Id}, {"t", list_to_binary(atom_to_list(Type))}, {"d", format_data(Type, Data)}]};
format_message(_) -> [].


start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

init([]) ->
    {ok, #state{}}.



% our code:
% #interface#
% login: 登录， 后面模块判断是否新建一个群控制器
% broadcast: 广播， 在消息协议里表示广播还是发给某客户端
% leave: 离开

proxy(Body, Req) ->
  % do not try catch here . let it crash if not match
  [{"user_id", Userid}, {"msg_head", MsgHead}, {"msg_body", MsgBody}] = Body,
  case MsgHead of
    <<"login">> -> 
      login(Userid, Req);
    <<"broadcast">> -> 
      broadcast(Userid, MsgBody, Req);
    <<"leave">> -> 
      leave(Userid, MsgBody, Req);
    <<"get_scene_list">> ->
      get_scene_list(Req)
  end.

login(Userid, MsgBody, Req) -> gen_server:cast(?MODULE, {login, Userid, Req}).
broadcast(Userid, MsgBody, Req) -> gen_server:cast(?MODULE, {broadcast, Userid, MsgBody, Req}).
leave(Userid, MsgBody, Req) -> gen_server:cast(?MODULE, {leave, Userid, Req}).
get_scene_list(Req) -> gen_server:cast(?MODULE, {get_scene_list, Req}).

handle_cast({login, Userid, MsgBody, Req}, State) ->
    breath_util:debug(["proxy receive msg", Userid, "login"]),
    {Sceneid} = MsgBody,
    case breath_master:login(Userid) of 
        {ok, Sceneid, Userlist} ->
            Req ! {response, {ok, Userlist}};
        {error, "too many connector"} ->
            Req ! {response, "too many connector"};
        {_} ->
            Req ! {response, "unhandled msg"}
    end.
    
handle_cast({broadcast, Userid, MsgBody, Req}, State) ->
    breath_util:debug(["proxy receive msg", Userid, "broadcast"]),
    {Sceneid, Msg} = MsgBody,
    case breath_scene:broadcast(Userid, Sceneid, Msg) of 
        {ok} ->
            Req ! {ok};
        {error, Why} ->
            Req ! {error, Why}
    end.
    
handle_cast({leave, Userid, MsgBody, Req}) ->
    breath_util:debug(["proxy receive msg", Userid, "leave"]),
    {Sceneid} = MsgBody,
    case breath_scene:leave(Userid, Sceneid, Msg) of 
        {ok} ->
            Req ! {ok};
        {error, Why} ->
            Req ! {error, Why}
    end.
    
handle_cast({get_scene_list, Req}) ->
    breath_util:debug(["proxy receive msg", Userid, "get_scene_list"]),
    {Sceneid} = MsgBody,
    case breath_scene:get_scene_list() of 
        {ok, Scene_list} ->
            Req ! {ok, Scene_list};
        {error, Why} ->
            Req ! {error, Why}
    end.


handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

