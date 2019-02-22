-module(ai_headers_handler).


-export([execute/2]).
-spec execute( cowboy:req(), cowboy_middleware:env() ) -> 
	{ ok, cowboy:req(), cowboy_middleware:env() } 	|
	{ suspend, module(), atom(), [any()] } 			|
	{ stop, cowboy:req() }.
execute( Req, Env = #{ ai_headers := Headers} ) ->
	execute( Req, Env,Headers);

%%
%%	Missing env keys
%%
execute( Req, Env ) ->
	{ ok, Req, Env }.

execute(Req,Env,Headers)->
	Req1 = cowboy_req:set_resp_headers(Headers,Req),
  {ok,Req1,Env}.
