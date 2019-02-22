-module(ai_session_handler).


-export([execute/2]).

-export([get_session/0,create_session/1,delete_session/0,update_session/1]).

-callback recover_session(cowboy_req:req(), term())
	-> {ok,term()} | not_found | reject.
-callback create_session(term(),term())
	-> {ok,term()} | {error,fail}.
-callback delete_session(term(),term())
	-> ok | {error,fail}.
-callback update_session(term(),term(),term())
	-> ok | {ok,term()} | {error,fail}.
-callback get_session(term(),term())
	-> {ok,term()} | not_found.




-spec execute( cowboy:req(), cowboy_middleware:env() ) -> 
	{ ok, cowboy:req(), cowboy_middleware:env() } 	|
	{ suspend, module(), atom(), [any()] } 			|
	{ stop, cowboy:req() }.
execute( Req, Env = #{ ai_session := SSO, handler := Handler,handler_opts := HandlerOpts} ) ->
	%% {module,Handler} = code:ensure_loaded(Handler),
	create_context(SSO),
	execute( Req, Env,Handler,HandlerOpts );

%%
%%	Missing env keys
%%
execute( Req, Env ) ->
	{ ok, Req, Env }.

execute( Req, Env,Handler,HandlerOpts ) ->
	case authorization_required(Handler,Req,HandlerOpts) of
		false -> 
			{ok,Req,Env};
		true ->
			recover_session(Req,Env)
	end.

authorization_required(Handler,Req,State)->
    case erlang:function_exported( Handler, authorization_required, 2 ) of
		true -> 
			Handler:authorization_required(Req,State);
		false -> 
			false
	end.
recover_session(Req,Env)->
	Ctx = get_context(),
	Handler = maps:get(handler,Ctx,undefined),
	Secret = maps:get(secret,Ctx,undefined),
	Redirect = maps:get(redirect,Ctx,undefined),
	case Handler:recover_session(Req,Secret) of 
		{ok,Token}->
			update_context(token,Token),
			{ ok, Req, Env };
		Error -> 
			fail(Redirect,Error,Req)
	end.

fail({ redirect, To },_Error,Req)->
	{ stop, cowboy_req:reply( 302, #{ <<"Location">> => To }, <<>>, Req ) };
fail(_,not_found,Req)-> {stop,cowboy_req:reply(401,#{},<<>>,Req)};
fail(_,invalid_token,Req)->{stop,cowboy_req:reply(401,#{},<<>>,Req)};
fail(_,expired,Req) -> {stop,cowboy_req:reply(401,#{},<<>>,Req)}.

create_context(Ctx)-> erlang:put(?MODULE,Ctx).

get_context()-> erlang:get(?MODULE).

update_context(token,Token)->
	case get_context() of 
		undefined -> erlang:put(?MODULE,#{token => Token});
		Ctx -> erlang:put(?MODULE, maps:put(token,Token,Ctx))
	end.

get_session()->
	case get_context() of 
		undefined -> undefined;
		Ctx -> 
			#{handler := Handler,secret := Secret } = Ctx,
			Token = maps:get(token,Ctx,undefined),
			Handler:get_session(Token,Secret)
	end.
create_session(Payload)->
	case get_context() of 
		undefined -> {error,fail};
		Ctx -> 
			#{handler := Handler,secret := Secret } = Ctx,
			case Handler:create_session(Payload,Secret) of 
				{ok,Token}-> 
					erlang:put(?MODULE,maps:put(token,Token,Ctx)),
					{ok,Token};
				_ ->  {error,fail}
			end
	end.
delete_session()->
	case get_context() of 
		undefined -> {error,fail};
		Ctx ->
			#{handler := Handler,secret := Secret } = Ctx,
			Token = maps:get(token,Ctx,undefined),
			case Handler:delete_session(Token,Secret) of 
				ok ->
					erlang:put(?MODULE, maps:remove(token,Ctx)),
					ok;
				_ -> {error,fail}
			end
	end.
update_session(Payload)->
	case get_context() of
		undefined -> {error,fail};
		Ctx ->
			#{handler := Handler,secret := Secret } = Ctx,
			Token = maps:get(token,Ctx,undefined),
			case Handler:update_session(Token,Payload,Secret) of 
				ok -> ok;
				{ok,Token0} -> 
              erlang:put(?MODULE, maps:put(token,Token0,Ctx)),
              {ok,Token0};
				_ -> {error,fail}
			end
	end.
