%%----------------------------------------------------
%% 游戏区监控树
%%----------------------------------------------------
-module(sup_boot).
-behaviour(supervisor).
-export([start_link/1, init/1]).

start_link(NodeType) ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, [NodeType]).

init([NodeType]) ->
    {ok, {
            {one_for_one, 50, 1}
            ,[
                {env, {env, start_link, ["env.cfg"]}, transient, 10000, worker, [env]}
                ,{sys_code, {sys_code, start_link, []}, transient, 10000, worker, [sys_code]}
                ,{sys_rand, {sys_rand, start_link, []}, transient, 10000, worker, [sys_rand]}
                ,{sys_boot, {sys_boot, start_link, [NodeType]}, transient, 10000, worker, [sys_boot]}
            ]
        }
    }.
