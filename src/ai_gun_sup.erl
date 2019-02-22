-module(ai_gun_sup).
-behaviour(supervisor).

-export([start_link/1,start_workers/1]).
-export([init/1]).

-define(SERVER,?MODULE).

start_link(Args) ->
	supervisor:start_link({local, ?MODULE}, ?MODULE, Args).

start_workers({Name,Opts})->
    PoolSize = maps:get(pool_size,Opts,5),
    MaxOverflow = maps:get(max_overflow,Opts,0),
    PoolBoyOpts =
        [{name,{local,Name}},{size,PoolSize},
         {max_overflow,MaxOverflow},{worker_module,ai_gun_worker}
        ],
    PoolSpec = ai_pool:pool_spec(Name,PoolBoyOpts,Opts),
    supervisor:start_child(?SERVER,PoolSpec).

init(Conf) ->
    SupFlags = #{strategy => one_for_one,
                 intensity => 1,
                 period => 5},
    PoolSpecs =
        lists:map(fun({Name,Opts})->
                          PoolSize = maps:get(pool_size,Opts,5),
                          MaxOverflow = maps:get(max_overflow,Opts,0),
                          PoolBoyOpts =
                              [{name,{local,Name}},{size,PoolSize},
                               {max_overflow,MaxOverflow},
                               {worker_module,ai_gun_worker}
                              ],
                          ai_pool:pool_spec(Name,PoolBoyOpts,Opts)
                  end,Conf),
    {ok, {SupFlags, PoolSpecs}}.
