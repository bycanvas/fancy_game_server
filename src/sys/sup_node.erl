%%----------------------------------------------------
%% 节点控制服监控树
%%----------------------------------------------------
-module(sup_node).
-behaviour(supervisor).
-export([start_link/0, init/1]).

-include("common.hrl").

start_link() ->
    Type = env:get(node_type),
    supervisor:start_link({local, ?MODULE}, ?MODULE, [Type]).

%% Shell服(用于安装和调试)
init([shell]) ->
    ?DEBUG("节点[Shell]相关服务开始启动"),
    {ok, {
            {one_for_one, 50, 1}
            ,[
            ]
        }
    };
%% 游戏服
init([zone]) ->
    ?DEBUG("节点[zone]相关服务开始启动"),
    {ok, {
            {one_for_one, 50, 1}
            ,[]
        }
    }.
