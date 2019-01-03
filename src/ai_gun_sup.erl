-module(ai_gun_sup).
-behaviour(supervisor).

-export([start_link/1,start_workers/1]).
-export([init/1]).

-define(SERVER,?MODULE).

start_link(Args) ->
	supervisor:start_link({local, ?MODULE}, ?MODULE, Args).

start_workers({Name,Opts})->
    PoolSize = maps:get(pool_size,Opts,5),
    PoolBoyOpts =
        [{name,{local,Name}},{size,PoolSize},
         {max_overflow,0},{worker_module,ai_gun_worker}
        ],
    PoolSepc = poolboy:child_spec(Name,PoolBoyOpts,Opts),
    case erlang:whereis(?SERVER) of 
        undefined ->
            {ok,_Pid} = ai_postgres_sup:start_link(), 
            supervisor:start_child(?SERVER,PoolSepc);
        _ -> supervisor:start_child(?SERVER,PoolSepc)
    end.


init(Conf) ->
    SupFlags = #{strategy => one_for_one,
                 intensity => 1,
                 period => 5},
    PoolSpecs =
        lists:map(fun({Name,Opts})->
                          PoolSize = maps:get(pool_size,Opts,5),
                          PoolBoyOpts =
                              [{name,{local,Name}},{size,PoolSize},
                               {max_overflow,0},{worker_module,ai_gun_worker}
                              ],
                          poolboy:child_spec(Name,PoolBoyOpts,Opts)
                  end,Conf),
    {ok, {SupFlags, PoolSpecs}}.