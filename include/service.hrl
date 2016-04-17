%%----------------------------------------------------
%% 服务数据结构
%%
%% @author wprehard@qq.com
%% @end
%%----------------------------------------------------

%% 服务定义
-record(service, {
        %% 服务名称
        id                  :: atom()
        %% 该服务的中文名称
        ,name = <<>>        :: bitstring()
        %% 启动函数
        ,mfa                :: undefined | {atom(), atom(), list()}
        %% 是否可同时运行多个
        ,multi = false      :: boolean()
        %% 是否世界类型的服务
        ,world = false      :: boolean()
        %% 是否依赖于其它服务
        ,depend_on = []     :: list()
        %% 是否可单独关闭
        ,can_stop = false   :: boolean()
        %% 是否允许监控树自动重启
        ,restart = transient    :: permanent | transient | temporary
        %% 关闭等待超时（单位：毫秒）
        ,shutdown = 300000      :: brutal_kill | pos_integer() | infinity
        %% 类型，worker:工作进程 supervisor:监控树
        ,type = worker      :: worker | supervisor
    }
).
