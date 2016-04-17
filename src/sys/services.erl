%%----------------------------------------------------
%% 服务配置数据
%%----------------------------------------------------
-module(services).
-export([
        config/1
        ,get/1
    ]
).

-include("service.hrl").

%% @doc 读取默认配置
-spec config(atom()) -> {ok, list()} | {error, undefined}.
config(Type) ->
    case cfg(Type) of
        {ok, L} -> {ok, parse(Type, L, [])};
        Else -> Else
    end.

%% 节点启动配置
-spec cfg(atom()) -> {ok, list()} | {error, undefined}.
cfg(shell) ->
    {ok, [test]};

%% 游戏节点
cfg(zone) ->
    {ok, [
            sup_node, 
            map_mgr, role_data, role_query, role_mgr,

            %% ----以下必须最后启动-----
            sup_acceptor, sys_listener
        ]};

%% 测试节点
cfg(test) ->
    {ok, [
            test
        ]};

cfg(_) ->
    {error, undefined}.

%% @doc 获取指定服务的配置数据
-spec get(atom()) -> {ok, #service{}} | {error, undefined}.
get(sup_node = Id) ->
    {ok, #service{
            id = Id
            ,name = "节点服务监控树"
            ,mfa = {sup_node, start_link, []}
            ,type = supervisor
        }
    };
get(sys_db = Id) ->
    {ok, #service{
            id = Id
            ,name = "数据库服务"
            ,mfa = {sys_db, start_link, []}
        }
    };
get(sup_acceptor = Id) ->
    {ok, #service{
            id = Id
            ,name = "acceptor监控树"
            ,mfa = {sup_acceptor, start_link, []}
            ,type = supervisor
        }
    };
get(sys_listener = Id) ->
    {ok, #service{
            id = Id
            ,name = "socket监听服务"
            ,mfa = {sys_listener, start_link, []}
            ,depend_on = [env, sup_acceptor]
        }
    };
get(role_data = Id) ->
    {ok, #service{
            id = Id
            ,name = "角色数据存取服务"
            ,mfa = {role_data, start_link, []}
        }
    };
get(role_mgr = Id) ->
    {ok, #service{
            id = Id
            ,name = "角色管理服务"
            ,mfa = {role_mgr, start_link, []}
        }
    };
get(role_query = Id) ->
    {ok, #service{
            id = Id
            ,name = "角色查询服务"
            ,mfa = {role_query, start_link, []}
        }
    };
get(map_mgr = Id) ->
    {ok, #service{
            id = Id
            ,name = "地图管理系统"
            ,mfa = {map_mgr, start_link, []}
        }
    };
get(test = Id) ->
    {ok, #service{
            id = Id
            ,name = "测试管理系统"
            ,mfa = {test, start_link, []}
        }
    };

get(_Name) ->
    {error, undefined}.

%% 统一处理成带参数的格式
parse(_Type, [], L) -> lists:reverse(L);
%% parse([{Id, Args} | T], L) -> parse(T, [{Id, Args} | L]);
parse(Type, [Id | T], L) -> parse(Type, T, [{Id, []} | L]).
