%%%-------------------------------------------------------------------
%%% @author  <david@laptop-02.local>
%%% @copyright (C) 2019, 
%%% @doc
%%%
%%% @end
%%% Created :  3 Jan 2019 by  <david@laptop-02.local>
%%%-------------------------------------------------------------------
-module(ai_gun_worker).

-behaviour(gen_server).
-behaviour(poolboy_worker).

%% API
-export([start_link/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	terminate/2, code_change/3, format_status/2]).

-export([run/2]).

-define(SERVER, ?MODULE).
-define(HTTP_TIMEOUT,3000).
-record(state, {
	host :: string(),
	port :: integer(),
	opts :: maps:maps(),
	conn = [] :: [],
	sender = undefined :: atom(),
	receiver = undefined :: tuple(),
	tasks :: maps:maps(),
	timers :: maps:maps(),
	monitors :: maps:maps()
}).

%%%===================================================================
%%% API
%%%===================================================================
run(Pid,Task)->
    gen_server:call(Pid,{run,Task}).
%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%% @end
%%--------------------------------------------------------------------
-spec start_link(Args :: maps:maps()) -> {ok, Pid :: pid()} |
	{error, Error :: {already_started, pid()}} |
	{error, Error :: term()} |
	ignore.
start_link(Args) ->
	gen_server:start_link(?MODULE, Args, []).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Initializes the server
%% @end
%%--------------------------------------------------------------------
-spec init(Args :: term()) -> {ok, State :: term()} |
	{ok, State :: term(), Timeout :: timeout()} |
	{ok, State :: term(), hibernate} |
	{stop, Reason :: term()} |
	ignore.
init(Args) ->
	%% process_flag(trap_exit, true),
	Host = maps:get(host,Args),
	Port = maps:get(port,Args),
	Opts = maps:get(opts,Args,#{}),
	{ok, #state{host = Host,port = Port,opts = Opts,conn = [],
				sender = undefined,tasks = maps:new(),
				timers = maps:new(),monitors = maps:new()	
		}}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling call messages
%% @end
%%--------------------------------------------------------------------
-spec handle_call(Request :: term(), From :: {pid(), term()}, State :: term()) ->
	{reply, Reply :: term(), NewState :: term()} |
	{reply, Reply :: term(), NewState :: term(), Timeout :: timeout()} |
	{reply, Reply :: term(), NewState :: term(), hibernate} |
	{noreply, NewState :: term()} |
	{noreply, NewState :: term(), Timeout :: timeout()} |
	{noreply, NewState :: term(), hibernate} |
	{stop, Reason :: term(), Reply :: term(), NewState :: term()} |
	{stop, Reason :: term(), NewState :: term()}.
handle_call({run,Task},From,State)->
		run(From,Task,State);
handle_call(_Request, _From, State) ->
	Reply = ok,
	{reply, Reply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling cast messages
%% @end
%%--------------------------------------------------------------------
-spec handle_cast(Request :: term(), State :: term()) ->
	{noreply, NewState :: term()} |
	{noreply, NewState :: term(), Timeout :: timeout()} |
	{noreply, NewState :: term(), hibernate} |
	{stop, Reason :: term(), NewState :: term()}.
handle_cast(_Request, State) ->
	{noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling all non call/cast messages
%% @end
%%--------------------------------------------------------------------
-spec handle_info(Info :: timeout() | term(), State :: term()) ->
	{noreply, NewState :: term()} |
	{noreply, NewState :: term(), Timeout :: timeout()} |
	{noreply, NewState :: term(), hibernate} |
	{stop, Reason :: normal | term(), NewState :: term()}.
handle_info({timeout,TimerRef,Key}, #state{receiver = From, timers = Timers} = State)->
    case maps:get(Key,Timers,undefined) of 
        undefined-> {noreply,State};
        Timer ->
            case ai_timer:is_previous(TimerRef,Timer) of 
                true -> {noreply,State}; %% an old timer is fired
                _->
                    NewState = clean(State),
                    if 
                        From  == undefined -> ok;
                        true -> gen_server:reply(From,{error,backend_fail})
                    end,
                    {noreply,NewState}
            end
    end;
handle_info({gun_response, ConnPid, StreamRef, fin, Status, Headers}, State) -> 
    Key = {ConnPid,StreamRef},
    State0 = update_header(Key,Status,Headers,State),
    State1 = teardown(Key,State0),
    State2 = done(State1),
    {noreply,State2};

handle_info({gun_response, ConnPid, StreamRef, nofin, Status, Headers}, State) -> 
    Key = {ConnPid,StreamRef},
    State0 = restart_timer(Key,State),
    State1 = update_header(Key,Status,Headers,State0),
    {noreply,State1};
handle_info({gun_data, ConnPid, StreamRef, nofin, Data},State)->
    Key = {ConnPid,StreamRef},
    State0 = restart_timer(Key,State),
    State1 = update_body(Key,Data,State0),
		{noreply,State1};
handle_info({gun_data, ConnPid, StreamRef, fin, Data},State)->
    Key = {ConnPid,StreamRef},
    State0 = update_body(Key,Data,State),
    State1 = teardown(Key,State0),
    State2 = done(State1), 
    {noreply,State2};

handle_info({'DOWN', _MRef, process, Pid, _Reason}, State )->
    State1 = gun_down(Pid,State),
    {noreply,State1};

handle_info({gun_down, ConnPid, _Protocol,
             _Reason, _Killed, _Unprocessed}, State) ->
    State1 = gun_down(ConnPid,State),
    {noreply,State1};
handle_info(_Info, State) ->
	{noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_server terminates
%% with Reason. The return value is ignored.
%% @end
%%--------------------------------------------------------------------
-spec terminate(Reason :: normal | shutdown | {shutdown, term()} | term(),
	State :: term()) -> any().
terminate(_Reason, _State) ->
	ok.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert process state when code is changed
%% @end
%%--------------------------------------------------------------------
-spec code_change(OldVsn :: term() | {down, term()},
	State :: term(),
	Extra :: term()) -> {ok, NewState :: term()} |
	{error, Reason :: term()}.
code_change(_OldVsn, State, _Extra) ->
	{ok, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called for changing the form and appearance
%% of gen_server status when it is returned from sys:get_status/1,2
%% or when it appears in termination error logs.
%% @end
%%--------------------------------------------------------------------
-spec format_status(Opt :: normal | terminate,
	Status :: list()) -> Status :: term().
format_status(_Opt, Status) ->
	Status.

%%%===================================================================
%%% Internal functions
%%%===================================================================
run(From,Task,State)->
	Result =
		lists:foldl(fun(I,Acc)->
			case Acc of 
				{stop,_NewState}-> Acc;
				{noreply,NewState}-> request(I,connect(NewState))
			end
		end,{noreply,State},Task),
    case Result of 
        {stop,NewState}-> {reply,{error,backend_fail},clean(NewState)};
        {noreply,NewState} -> {noreply,NewState#state{receiver = From}}
    end.
clean(#state{conn = Conn,sender = Sender, timers = Timers, monitors = M} = State)->
	lists:foreach(fun({ConnPid,_} = Key) ->
		try
			case maps:get(Key,Timers,undefined) of
				undefined -> ok;
				Timer -> ai_timer:cancel(Timer)
			end,
			if 
				Sender == undefined -> 
						ai_process:demonitor_process(ConnPid,M),
						gun:close(ConnPid),
						gun:flush(ConnPid);
					true -> ok
			end
		catch
			_Error:_Reason -> ok
		end
	end,Conn),
	if 
			Sender == undefined-> ok;
			true -> 
					ai_process:demonitor_process(Sender,M),
					gun:close(Sender),
					gun:flush(Sender)
	end,
  State#state{conn = [],sender = undefined, receiver = undefined,tasks = maps:new(),
			timers = maps:new(), monitors = maps:new()}.

connect(#state{host = Host,port = Port,opts = Opts, sender = Sender,monitors = M} = State)->
	if 
		Sender == undefined ->
			{ok,ConnPid} = gun:open(Host, Port,Opts),
			case gun:await_up(ConnPid) of
				{error,_Reason} -> {stop,State};
				{ok,Protocol} -> 
					M1 = ai_process:monitor_process(ConnPid,M),
					if 
							Protocol == http2 ->
									{ConnPid,State#state{monitors = M1,sender = ConnPid}};
							true ->
									{ConnPid,State#state{monitors = M1,sender = undefined}}
					end
			end;
			true  -> {Sender,State}
	end.
request(_,{stop,State})-> {stop,State};
request({Action,TaskKey,URL}, {ConnPid,State})->
	Method = method(Action),
	StreamRef = gun:request(ConnPid,Method,URL,[{<<"content-type">>,<<"application/json;charset=UTF-8">>}]),
    Key = {ConnPid,StreamRef},
    gun_up(Key,TaskKey,State);
request({Action,TaskKey,URL,{body,Data}}, {ConnPid,State})->
	Method = method(Action),
	StreamRef = gun:request(ConnPid,Method,URL,[{<<"content-type">>,<<"application/json;charset=UTF-8">>}],Data),
    Key = {ConnPid,StreamRef},
    gun_up(Key,TaskKey,State);
request({Action,TaskKey,URL,{headers,Headers}}, {ConnPid,State})->
	Method = method(Action),
	StreamRef = gun:request(ConnPid,Method,URL,Headers),
    Key = {ConnPid,StreamRef},
    gun_up(Key,TaskKey,State);
request({Action,TaskKey,URL,{headers,Headers},{body,Data}}, {ConnPid,State})->
	Method = method(Action),
	StreamRef = gun:request(ConnPid,Method,URL,Headers,Data),
    Key = {ConnPid,StreamRef},
    gun_up(Key,TaskKey,State).
gun_up(Key,TaskKey,#state{opts = Opts,conn = Conn,tasks = Tasks, sender = undefined, timers = Timers} = State)->
	Timeout = maps:get(gun_timeout,Opts,?HTTP_TIMEOUT),
  	Timer1 = ai_timer:start(Timeout,Key,ai_timer:new([async])),
	{noreply,State#state{
							 conn = [Key | Conn], 
							 tasks = maps:put(Key,#{key => TaskKey,status => undefined,headers => [],body => <<>>},Tasks),
							 timers = maps:put(Key,Timer1,Timers)}
	};
gun_up(Key,TaskKey,#state{opts = Opts,conn = Conn,tasks = Tasks, sender = Sender, timers = Timers} = State)->
		Timeout = maps:get(gun_timeout,Opts,?HTTP_TIMEOUT),
		Timers0 = 
			case maps:get(Sender,Timers,undefined) of 
				undefined ->
					Timer1 = ai_timer:start(Timeout,Sender,ai_timer:new([async])),
					maps:put(Sender,Timer1,Timers);
				_-> Timers 
			end,
		{noreply,State#state{
							 conn = [Key | Conn], 
							 tasks = maps:put(Key,#{key => TaskKey,status => undefined,headers => [],body => <<>>},Tasks),
							 timers = Timers0
		}}.
update_header(Key,Status,Headers,#state{tasks = Tasks} = State)->
	Tasks0 = ai_maps:put([Key,headers],Headers,Tasks),
    State#state{
			tasks = ai_maps:put([Key,status],Status,Tasks0)
		}.
update_body(Key,Data,#state{tasks = Tasks} = State)->
    Buffer = ai_maps:get([Key,body],Tasks),
		State#state{
			tasks = ai_maps:put([Key,body],<<Buffer/binary,Data/binary>>,Tasks)
		 }.
restart_timer(Key,#state{timers = T,sender = undefined} = State)->
    T0 = 
        case maps:get(Key,T,undefined) of 
            undefined -> T;
            Timer ->
                Timer0 = ai_timer:restart(Timer),
                maps:put(Key,Timer0,T)
        end,
    State#state{timers = T0};
restart_timer(_Key,#state{timers = T,sender = Sender} = State) ->
	T0 = 
		case maps:get(Sender,T,undefined) of 
				undefined -> T;
				Timer ->
						Timer0 = ai_timer:restart(Timer),
						maps:put(Sender,Timer0,T)
		end,
	State#state{timers = T0}.		
	
teardown({ConnPid,_Ref} = Key,#state{conn = Conn,sender = undefined, timers = T, monitors = M} = State)->
    M0 = ai_process:demonitor_process(ConnPid,M),
    gun:close(ConnPid),
    T0 =
        case maps:get(Key,T,undefined) of
            undefined -> T;
            Timer -> 
                ai_timer:cancel(Timer),
                maps:remove(Key,T)
        end,
    Conn0 = lists:filter(fun(I)-> I /= Key end,Conn),
    State#state{
			conn = Conn0,
			timers = T0,
			monitors = M0
		 };
teardown(Key,#state{conn = Conn} = State) ->
		Conn0 = lists:filter(fun(I) -> I /= Key end,Conn),
		State#state{
			conn = Conn0
		}.

done(#state{conn = [],tasks = Tasks,receiver = From} = State)->
    Result = 
        lists:foldl(fun({_Key,Payload},Acc)->
													TaskKey = maps:get(key,Payload),
													Acc#{TaskKey => Payload}
												end,#{},maps:to_list(Tasks)),
    State0 = clean(State),
    gen_server:reply(From,{done,Result}),
    State0;
done(#state{conn = _Conn} = State) -> State.
gun_down(_ConnPid,#state{receiver = undefined} = State)-> clean(State);
gun_down(ConnPid,#state{conn = Conn,receiver = From} = State)->
    Found = lists:any(fun({Pid,_Ref})-> Pid == ConnPid end,Conn),
    if
        Found == true ->
						State0 = clean(State),
						gen_server:reply(From,{error,backend_fail}),
						State0;
        true ->
            State
    end.
method(get)-><<"GET">>;
method(post)-><<"POST">>;
method(patch)-><<"PATCH">>;
method(options)-><<"OPTIONS">>;
method(head)-><<"HEAD">>;
method(delete)-><<"DELETE">>;
method(put)-><<"PUT">>.