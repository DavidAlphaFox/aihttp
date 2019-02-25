-module(ai_gun_sup).
-behaviour(supervisor).

-export([start_link/0]).
-export([init/1]).

-define(SERVER,?MODULE).

start_link() ->
	supervisor:start_link({local, ?MODULE}, ?MODULE, []).


init([]) ->
    SupFlags = #{strategy => one_for_one,
                 intensity => 5,
                 period => 5},
    {ok, {SupFlags, []}}.
