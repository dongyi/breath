-module (breath_room).
-author ("Dong Yi <juvenpp@gmail.com>").
-behaviour(gen_server).

-export([start_link/0]).

-export([init/1, handle_call/3,handle_cast/2,handle_info/2,terminate/2,code_change/3]).

-export([join/2,leave/2,wait/3,wait_finish/2,chat_message/2,get_users/1,get_msg_id/2,find_idle_clients/0]).

% Time before a client is considered gone
-define(MAX_IDLE_TIME, 160).
-define(CHECK_IDLE_TIME, 60).

% Rate limiting for messages
-define(RATE_MSG_INTERVAL, 3).
-define(RATE_MSG_COUNT, 10).

% Max connections per host
-define(MAX_CONNECTIONS, 3).

-define(ADMIN_PASSWORD, "dongy1").

-include_lib("stdlib/include/qlc.hrl").

-record(client_state, {
id, uid, host,last_action,admin=false,last_msg=never,msg_count=0
}).

-record(state, {
clients=[],bans_table
}).

-record(user_ban, {
host,last_uid,reason,until
}).

start_link() -> gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

join(UserId, Host) -> gen_server:call(?MODULE, {join, {UserId, Host}}, infinity).
leave(Sess, Reason) -> gen_server:cast(?MODULE, {leave, {Sess, Reason}}).
chat_message(Sess, Msg) -> gen_server:cast(?MODULE, {chat_message, {Sess, Msg}}).
wait(Sess, MsgID, Pid) -> gen_server:cast(?MODULE, {wait, {Sess, MsgID, Pid}}).
wait_finish(Sess, Pid) -> gen_server:cast(?MODULE, {wait_finish, {Sess, Pid}}).
get_users(Sess) -> gen_server:call(?MODULE, {get_users, Sess}, infinity).
get_msg_id(Sess, Pid) -> gen_server:cast(?MODULE, {get_msg_id, {Sess, Pid}}).
find_idle_clients() -> gen_server:cast(?MODULE, find_idle_clients).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  breath_room helpers
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
is_banned(Host, #state{bans_table=Table}) ->
    Now = now(),
    case qlc:e(qlc:q([Ban || Ban <- dets:table(Table), Ban#user_ban.host == Host])) of
        [] -> no;
        [#user_ban{until=Until} | _] when Until >= Now -> {yes, Until};
        _ -> no
    end.

validate_uid([], _) -> {error, bad_format};
validate_uid(UserId, #state{clients=Clients}) ->
    Shortened = list_to_binary(lists:sublist(string:strip(UserId), 16)),
    case {re:run(binary_to_list(Shortened), "^([A-Za-z0-9]+)$"), lists:filter(fun(#client_state{uid=N}) -> N == Shortened end, Clients)} of
        {{match, _}, []} -> {ok, Shortened};
        {nomatch, _} -> {error, bad_format};
        _ -> {error, not_available}
    end.
    
get_unique_session(#state{clients=Clients} = State) ->
	X = breath_util:generate_hash(),
	case lists:filter(fun(#client_state{id=ID}) -> ID == X end, Clients) of % Make sure the session ID is unique
		[] -> X;
		_ -> get_unique_session(State)
	end.
	
get_session(Session, #state{clients=Clients}) ->
	case lists:filter(fun(#client_state{id=ID}) -> ID == Session end, Clients) of
		[] -> {error, not_found};
		[X|_] -> {ok, X}
	end.

get_user_by_uid(UserId, State) when is_list(UserId) -> get_user_by_uid(list_to_binary(UserId), State);
get_user_by_uid(UserId, #state{clients=Clients}) when is_binary(UserId) ->
	case lists:filter(fun(#client_state{uid=N}) -> N == UserId end, Clients) of
		[] -> {error, not_found};
		[X|_] -> {ok, X}
	end;
get_user_by_uid(_, _) -> {error, not_found}.

send_system_msg(Client, Msg) when is_binary(Msg) -> breath_postoffice:send_mail(Client#client_state.id, {msg, {system_msg, Msg}});
send_system_msg(Client, Msg) when is_list(Msg) -> send_system_msg(Client, list_to_binary(Msg));
send_system_msg(_, _) -> ok.

should_rate_limit(#client_state{last_msg=never} = C, State) -> 
    NewState = update_client(C#client_state{last_msg=now(),msg_count=1}, State),
    {no, NewState};
should_rate_limit(#client_state{last_msg=LastMsg,msg_count=Count} = C, State) ->
	RateSecs = calendar:datetime_to_gregorian_seconds(calendar:now_to_datetime(now())) - calendar:datetime_to_gregorian_seconds(calendar:now_to_datetime(LastMsg)),
	case {RateSecs =< ?RATE_MSG_INTERVAL, (Count + 1) > ?RATE_MSG_COUNT} of
	    {true, true} -> {yes, update_client(C#client_state{msg_count=Count}, State)};
	    {false, _} -> {no, update_client(C#client_state{msg_count=0}, State)};
	    _ -> {no, update_client(C#client_state{msg_count=Count+1}, State)}
	end;
should_rate_limit(_, State) -> {no, State}.

% Decrements a host
host_disconnected(Host) ->
	case ets:lookup(conns, Host) of
		[] -> ok;
		[{_, Num}] when is_integer(Num) -> ets:insert(conns, {Host, Num - 1});
		_ -> ets:insert(conns, {Host, 0})
	end.

% Updates a host in connections ETS table
update_host_conns(Host, Conns) -> ets:insert(conns, {Host, Conns}).

% Looks up host in connections ETS table
can_connect(Host) ->
	case ets:lookup(conns, Host) of
		[] -> {yes, 0};
		[{_, Num}] when Num < ?MAX_CONNECTIONS -> {yes, Num};
		_ -> no
	end.
		

update_client(Client, #state{clients=Clients} = State) -> 
    NewClient = Client#client_state{last_action=now()},
    Others = lists:filter(fun(#client_state{id=ID}) -> ID /= Client#client_state.id end, Clients),
    {NewClient, State#state{clients=[NewClient | Others]}}.


% Get information about a user.
process_admin_command(["info", User], Client, State) ->
    case get_user_by_uid(User, State) of
        {error, not_found} -> send_system_msg(Client, "Unknown user.");
        {ok, #client_state{last_action=LastAction,uid=UserId,host=Host}} ->
            DateStr = httpd_util:rfc1123_date(calendar:now_to_local_time(LastAction)),
            Args = [{uid, UserId}, {ip_address, Host}, {last_action, DateStr}],
            send_system_msg(Client, breath_util:get_template("admin_info", Args))
    end,
    {ok, State};

% Admin Logout.
process_admin_command(["logout"], Client, State) ->
    breath_postoffice:broadcast_mail({msg, {admin_logged_out, Client#client_state.uid}}, []),
    {_, NewState} = update_client(Client#client_state{admin=false}, State),
    {ok, NewState};

% Kick a user
process_admin_command(["kick", User, Reason], #client_state{id=MySess} = Client, State) ->
    case get_user_by_uid(User, State) of
        {error, not_found} -> send_system_msg(Client, "Unable to kick: user not found.");
        {ok, #client_state{id=Sess} = C} when Sess /= MySess -> 
            send_system_msg(C, "You have been kicked from the chat room. Reason: " ++ Reason),
            breath_room:leave(Sess, "kicked: " ++ Reason);
        _ -> send_system_msg(Client, "Sorry, you can't kick yourself!")
    end,
    {ok, State};
    
% Unban a user
process_admin_command(["unban", Host], Client, State) ->
	case qlc:e(qlc:q([X || X <- dets:table(State#state.bans_table), X#user_ban.host == Host])) of
		[] -> send_system_msg(Client, "Unable to unban: Invalid host.");
		[Obj|_] -> dets:delete_object(State#state.bans_table, Obj), send_system_msg(Client, "Removed ban.")
	end,
	{ok, State};

% List bans
process_admin_command(["bans"], Client, State) ->
	Bans = qlc:e(qlc:q([X || X <- dets:table(State#state.bans_table)])),
	Args = [{bans, lists:map(fun(#user_ban{host=Host,reason=Reason,last_uid=UserId,until=Until}) -> [{host, Host}, {reason, Reason}, {uid, UserId}, {time, breath_util:time_interval_str(Until)}] end, Bans)}],
	send_system_msg(Client, breath_util:get_template("admin_bans", Args)),
	{ok, State};

% Ban a user
process_admin_command(["ban", User, Reason, Seconds], #client_state{id=MySess} = Client, State) ->
    case {get_user_by_uid(User, State), string:to_integer(Seconds)} of
        {_, {error, _}} -> send_system_msg(Client, "Unable to ban: invalid time specified.");
        {{error, not_found}, _} -> send_system_msg(Client, "Unable to ban: user not found.");
        {{ok, #client_state{id=Sess,host=Host} = C}, {Secs, _}} when Sess /= MySess -> 
            {Mega, S, Micro} = now(),
            BanTime = {Mega, S + Secs, Micro},
            BanTimeStr = httpd_util:rfc1123_date(calendar:now_to_local_time(BanTime)),
            send_system_msg(C, "You have been banned from the chat room until " ++ BanTimeStr ++ ". Reason: " ++ Reason),
            breath_room:leave(Sess, "banned: " ++ Reason),
            dets:insert(State#state.bans_table, #user_ban{host=Host,last_uid=User,reason=Reason,until=BanTime});
        _ -> send_system_msg(Client, "Sorry, you can't ban yourself!")
    end,
    {ok, State};
    
% Get help for administrator    
process_admin_command(["help"], Client, State) ->
    send_system_msg(Client, "Admin Help: (/admin logout) (/admin info user) (/admin kick user reason) (/admin ban user reason seconds) (/admin bans) (/admin unban host)"),
    {ok, State};
    
% Unknown command, just let the user know.
process_admin_command(_, Client, State) -> send_system_msg(Client, "Unknown admin command. Type /admin help."), {ok, State}.

process_command("help", _, Client, State) ->
    send_system_msg(Client, "Commands: (/auth password)"),
    {ok, State};


% Generic admin handler.
process_command("admin", Args, #client_state{admin=true} = Client, State) -> process_admin_command(Args, Client, State);
    
% Authenticate an administrator    
process_command("auth", [?ADMIN_PASSWORD], Client, State) -> 
    send_system_msg(Client, "You are now authenticated as an administrator. Type /admin help for commands."),
    case Client#client_state.admin of
        false -> breath_postoffice:broadcast_mail({msg, {admin_logged_in, Client#client_state.uid}}, []);
        _ -> ok
    end,
    {_, NewState} = update_client(Client#client_state{admin=true}, State),
    {ok, NewState}; 
process_command("auth", _, Client, State) -> send_system_msg(Client, "Invalid adminstrator password."), {ok, State};  
 
% Unknown command. 
process_command(_, _, Client, State) -> 
    send_system_msg(Client, "Invalid command or format."),
    {ok, State}.

get_params(X) when is_list(X) ->
    Opts = [{capture, [param,rest], list}],
    case {re:run(X, "^\\\"(?<param>[^\\\"\\\\]*(\\\\.[^\\\"\\\\]*)*)\\\"[ ]*(?<rest>.*)$", Opts), re:run(X, "^[ ]*(?<param>[A-Za-z0-9]+)[ ]*(?<rest>.*)$", Opts)} of
        {_, {match, [Param, Rest]}} -> [Param | get_params(Rest)];
        {{match, [Param, Rest]}, _} -> [Param | get_params(Rest)];
        _ -> []
    end;
get_params(_) -> [].
    
handle_command(Client, Msg, State) ->
    case re:run(Msg, "^/([A-Za-z0-9]+) ?(.+){0,1}", [{capture, all_but_first, list}]) of
        {match, [Cmd]} -> process_command(Cmd, [], Client, State);
        {match, [Cmd, ParamStr]} -> process_command(Cmd, get_params(ParamStr), Client, State);
        _ -> {error, no_command}
    end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  gen_server exports
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
init(_Args) -> 
	process_flag(trap_exit, true),
	{ok, BansTable} = dets:open_file("bans.dets", []),
	_ConnTable = ets:new(conns, [named_table]),
	timer:apply_after(?CHECK_IDLE_TIME, ?MODULE, find_idle_clients, []),
	{ok, #state{bans_table=BansTable}}.

handle_call({join, {UserId, Host}}, _From, #state{clients=Clients} = State) when is_list(UserId) ->
    case {is_banned(Host,State), can_connect(Host), validate_uid(UserId, State)} of
        {{yes, Until}, _, _} -> {reply, {error, {banned, Until}}, State};
		{_, no, _} -> {reply, {error, too_many_conns}, State};
		{_, _, {error, Reason}} -> {reply, {error, Reason}, State};
        {_, {yes, NumConns}, {ok, ValidUserId}} ->
            Session = get_unique_session(State),
            case breath_postoffice:create_mailbox(Session) of
                ok -> 
					update_host_conns(Host, NumConns + 1),
                    breath_postoffice:broadcast_mail({msg, {user_joined_room, ValidUserId}}, [Session]),
                    Client = #client_state{id=Session,uid=ValidUserId,host=Host,last_action=now()},
                    {reply, {ok, Session}, State#state{clients=[Client | Clients]}};
                {error, _} -> {reply, {error, not_available}, State}
            end
    end;

handle_call({get_users, Sess}, _From, State) ->
    case get_session(Sess, State) of
        {error, not_found} -> {reply, {error, not_found}, State};
        {ok, C} -> 
            {_, NewState} = update_client(C, State),
            {reply, {ok, lists:map(fun(#client_state{uid=UserId}) -> UserId end, State#state.clients)}, NewState}
    end;

handle_call(_Req, _From, State) -> {noreply, State}.

handle_cast({get_msg_id, {Sess,Pid}}, State) ->
    case get_session(Sess, State) of
        {error, not_found} -> 
            Pid ! {error, bad_session},
            {noreply, State};
        {ok, C} -> 
            {_, NewState} = update_client(C, State),
            breath_postoffice:send_mail(Sess, {get_msg_id, Pid}), {noreply, NewState}
    end;

handle_cast(find_idle_clients, #state{clients=Clients} = State) ->
	lists:foreach(fun(Client) ->
		LastAction = calendar:now_to_datetime(Client#client_state.last_action),
		Now = calendar:now_to_datetime(now()),
		IdleSecs =  calendar:datetime_to_gregorian_seconds(Now) - calendar:datetime_to_gregorian_seconds(LastAction),
		case IdleSecs > ?MAX_IDLE_TIME of
			true -> 
			    error_logger:info_msg("User timed out: ~p, secs: ~p", [Client#client_state.uid, IdleSecs]),
				timer:apply_after(0, ?MODULE, leave, [Client#client_state.id, "timeout"]);
			_ -> noop
		end
	end, Clients),
	timer:apply_after(?CHECK_IDLE_TIME, ?MODULE, find_idle_clients, []),
	{noreply, State};
    
handle_cast({wait, {Sess, MsgID, Pid}}, State) when is_integer(MsgID) ->
    case get_session(Sess, State) of
        {error, not_found} -> Pid ! {error, bad_session}, {noreply, State};
        {ok, C} ->
            {_, NewState} = update_client(C, State), 
            breath_postoffice:send_mail(Sess, {add_listener, {MsgID, Pid}}), {noreply, NewState}
    end;
    
handle_cast({wait_finish, {Sess, Pid}}, State) ->
    case get_session(Sess, State) of
        {error, not_found} -> {noreply, State};
        {ok, _} -> breath_postoffice:send_mail(Sess, {remove_listener, Pid}), {noreply, State}
    end;
    
handle_cast({chat_message, {Sess, Msg}}, State) when is_list(Msg) and (length(Msg) > 0) ->
    case get_session(Sess, State) of
        {error, not_found} -> {noreply, State};
        {ok, #client_state{uid=UserId,id=ID} = C} ->
            CleanMsg = breath_util:unicode_clean(lists:sublist(Msg, 256)),
            {NewC, NewState} = update_client(C, State),
            case handle_command(NewC, CleanMsg, NewState) of
                {ok, S1} -> {noreply, S1};
                _ ->
                    case should_rate_limit(NewC, NewState) of
                        {yes, {_, S2}} ->
                            send_system_msg(NewC, "You are sending messages to fast. Please wait a few seconds and try again."),
                            {noreply, S2};
                        {no, {RateClient, S2}} ->
                            breath_postoffice:broadcast_mail({msg, {chat_msg, {UserId, CleanMsg}}}, [ID]),
                            breath_postoffice:send_mail(ID, {msg, {sent_chat_msg, {UserId, CleanMsg}}}),
                            {_, S3} = update_client(RateClient#client_state{last_msg=now()}, S2),
                            {noreply, S3}
                    end
            end
    end;

handle_cast({leave, {Sess, Reason}}, #state{clients=Clients} = State) when is_list(Reason) ->
    case get_session(Sess, State) of
        {error, not_found} -> {noreply, State};
        {ok, Client} ->
			host_disconnected(Client#client_state.host),
            breath_postoffice:delete_mailbox(Client#client_state.id),
            CleanReason =  breath_util:unicode_clean(lists:sublist(Reason, 32)),
            breath_postoffice:broadcast_mail({msg, {user_left_room, {Client#client_state.uid, CleanReason}}}, [Client#client_state.id]),
            OtherClients = lists:filter(fun(#client_state{id=ID}) -> ID /= Client#client_state.id end, Clients),
            {noreply, State#state{clients=OtherClients}}
    end;    

handle_cast(_Request, State) -> {noreply, State}.
	
	
handle_info(_Info, State) -> {noreply, State}.
terminate(_Reason, _State) -> normal.

code_change(_OldVsn, State, _Extra) -> {ok, State}.
