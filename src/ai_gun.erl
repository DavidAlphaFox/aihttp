-module(ai_gun).
-export([start_pool/1]).

-define(SERVER,ai_gun_sup).

start_pool({Name,Opts})->
    PoolSize = maps:get(pool_size,Opts,5),
    MaxOverflow = maps:get(max_overflow,Opts,0),
    PoolBoyOpts =
        [{name,{local,Name}},{size,PoolSize},
         {max_overflow,MaxOverflow},{worker_module,ai_gun_worker}
        ],
    PoolSpec = ai_pool:pool_spec(Name,PoolBoyOpts,Opts),
    supervisor:start_child(?SERVER,PoolSpec).
