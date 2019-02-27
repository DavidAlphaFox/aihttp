-module(ai_session_handler).


-export([execute/2]).

-export([retrive/1,create/2,delete/1,update/2]).

-callback recover(cowboy_req:req(),Secret :: term()) -> 
							ok|{ok,cowboy_req:req()}
							|fail|{fail,cowboy_req:req()}.
-callback create(cowboy_req:req(), Payload :: term(),Secret :: term()) ->
							{ok,cowboy_req:req(), term()}
							|{ok,cowboy_req:req()}
							|{error,fail}.
-callback update(cowboy_req:req(), Payload:: term(),Secret::term()) ->
							{ok,cowboy_req:req(), term()}
							|{ok,cowboy_req:req()}
							|{error,fail}.
-callback retrive(cowboy_req:req(),Secret ::term())->
							{ok,cowboy_req:req(), term()} 
							|{error,not_found}.
-callback delete(cowboy_req:req(),Secret :: term()) ->
							{ok,cowboy_req:req(), term()}
							|{ok,cowboy_req:req()}
							|{error,fail}.



-spec execute( cowboy:req(), cowboy_middleware:env() ) ->
	{ ok, cowboy:req(), cowboy_middleware:env() } 	|
	{ suspend, module(), atom(), [any()] } 			|
	{ stop, cowboy:req() }.
execute( Req, Env = #{ ai_session := Context, handler := Handler,handler_opts := HandlerOpts} ) ->
	context(Context),
	execute( Req, Env,Handler,HandlerOpts );

%%
%%	Missing env keys
%%
execute( Req, Env ) ->
	{ ok, Req, Env }.

execute( Req, Env,Handler,HandlerOpts ) ->
	case session_required(Handler,Req,HandlerOpts) of
		false -> {ok,Req,Env};
		true -> recover(Req,Env)
	end.

session_required(Handler,Req,State)->
    case erlang:function_exported( Handler, session_required, 2 ) of
				true -> Handler:session_required(Req,State);
				false -> false
	end.

recover(Req,Env)->
	Ctx = context(),
	Handler = maps:get(handler,Ctx,undefined),
	Secret = maps:get(secret,Ctx,undefined),
	Redirect = maps:get(redirect,Ctx,undefined),
	if
		Handler  == undefined ->
			{stop,cowboy_req:reply(500,#{},<<"need a session handler">>,Req)};
		true ->
			case Handler:recover(Req,Secret) of
					ok -> {ok, Req, Env };
					fail -> fail(Redirect,Req);
					{ok,Req0} -> {ok,Req0,Env};
					{fail,Req0} -> fail(Redirect,Req0)
			end
	end.

fail({ redirect, To },Req)->
	{ stop, cowboy_req:reply( 302, #{ <<"Location">> => To }, <<>>, Req ) };
fail(_,Req)-> {stop,cowboy_req:reply(401,#{},<<>>,Req)}.


context()-> erlang:get(?MODULE).
context(Ctx)-> erlang:put(?MODULE,Ctx).


%%%% API

-spec create(cowboy_req:req(),Payload::term())->
	{ok,cowboy_req:req(),term()}|{ok,cowboy_req:req()}
	|{error,not_initialized}|{error,fail}.
create(Req,Payload)->
	case  context() of 
			undefined -> {error,not_initialized};
			Ctx -> 
				#{handler := Handler,secret := Secret } = Ctx,
				Handler:create(Req,Payload,Secret)
	end.
-spec retrive(cowboy_req:req())->
	{ok,cowboy_req:req(),term()}
	|{error,not_initialized}|{error,not_found}.
retrive(Req)->
		case context() of
				undefined ->{error,not_initialized};
				Ctx ->
						#{handler := Handler,secret := Secret } = Ctx,
						Handler:retrive(Req,Secret)	
		end.
-spec update(cowboy_req:req(),Payload::term())->
				{ok,cowboy_req:req(),term()}|{ok,cowboy_req:req()}
				|{error,not_initialized}|{error,fail}.
update(Req,Payload)->
		case context() of
				undefined -> {error,not_initialized};
				Ctx ->
						#{handler := Handler,secret := Secret } = Ctx,
						Handler:update(Req,Payload,Secret)
		end.
-spec delete(cowboy_req:req())->
	{ok,cowboy_req:req(),term()}|{ok,cowboy_req:req()}
	|{error,not_initialized}|{error,fail}.
delete(Req)->
	case context() of
			undefined -> {error,not_initialized};
			Ctx ->
				#{handler := Handler,secret := Secret } = Ctx,
					Handler:delete(Req,Secret)
	end.
