-module(breath_postoffice).
-behaviour(gen_server).

-export([start_link/0]).

-export([init/1, handle_call/3,handle_cast/2,handle_info/2,terminate/2,code_change/3]).

-export([create_mailbox/1, delete_mailbox/1, send_mail/2, broadcast_mail/2]).


-record(state, {
mailboxes=[]
}).

start_link() -> gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

create_mailbox(UserId) -> gen_server:call(?MODULE, {create_mailbox, UserId}).
delete_mailbox(UserId) -> gen_server:cast(?MODULE, {delete_mailbox, UserId}).
send_mail(UserId, Msg) -> gen_server:cast(?MODULE, {send_mail, {UserId, Msg}}).
broadcast_mail(Msg, Except) -> gen_server:cast(?MODULE, {broadcast_mail, {Msg, Except}}).


get_mailbox(UserId, #state{mailboxes=MBoxes}) ->
    case lists:filter(fun({UserId, _}) -> UserId == UserId end, MBoxes) of
        [] -> {error, notfound};
        [M|_] -> {ok, M}
    end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  gen_server exports
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
init(_Args) -> 
	process_flag(trap_exit, true),
	{ok, #state{}}.

handle_call({create_mailbox, UserId}, _From, #state{mailboxes=MBoxes} = State) ->
    case get_mailbox(UserId, State) of
        {ok, _} -> {reply, {error, already_exists}, State};
        {error, notfound} ->
            Pid = spawn_link(chat_mailbox, start, [UserId]),
            NewBox = {UserId, Pid},
            {reply, ok, State#state{mailboxes=[NewBox | MBoxes]}}
    end;
    	
handle_call(_Req, _From, State) -> {noreply, State}.


handle_cast({broadcast_mail, {Msg, Except}}, #state{mailboxes=MBoxes} = State) when is_list(Except) ->
    [Pid ! {mail, Msg} || {UserId, Pid} <- MBoxes, lists:member(UserId, Except) == false],
    {noreply, State};

handle_cast({send_mail, {UserId, Msg}}, State) ->
    case get_mailbox(UserId, State) of
        {ok, {_UserId, Pid}} -> Pid ! {mail, Msg};
        _ -> ok
    end,
    {noreply, State};

handle_cast({delete_mailbox, UserId}, #state{mailboxes=MBoxes} = State) ->
    NewBoxes = lists:filter(fun({UserId, Pid}) ->
        case UserId /= UserId of
            false -> 
                % tell the mailbox process to quit
                Pid ! quit, false;
            _ -> true
        end
    end, MBoxes),
    {noreply, State#state{mailboxes=NewBoxes}};
    
    
handle_cast(_Request, State) -> {noreply, State}.
	
	
handle_info(_Info, State) -> 
	%error_logger:info_msg("info: ~p", [Info]),
	{noreply, State}.
terminate(_Reason, _State) -> normal.

code_change(_OldVsn, State, _Extra) -> {ok, State}.
