%%----------------------------------------------------
%% 数据库写入队列监控树
%%----------------------------------------------------
-module(sup_db_queue).
-behaviour(supervisor).
-export([start_link/0, init/1]).
-include("common.hrl").

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
    ?INFO("[~w] 正在启动数据库写入队列监控树...", [?MODULE]),
    {ok, {
            {one_for_one, 50, 1},
            [
            ]
        }
    }.

