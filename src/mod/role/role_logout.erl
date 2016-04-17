%%----------------------------------------------------
%% 角色退出时需要调用的函数
%%----------------------------------------------------
-module(role_logout).
-export([
        do/1
    ]).
-include("common.hrl").
-include("role.hrl").

%% 需要调用的函数列表
%% 列表中的函数及返回格式要求如下:
%% fun(Role::#role{}) -> ok | {ok, NewRole::#role{}}.
-define(fun_list, [
        {role_mgr, logout}
    ]).

%% ----------------------------------------------------
%% 外部接口
%% ----------------------------------------------------
%% @doc 执行退出调用
-spec do(Role::#role{}) -> NewRole::#role{}.
do(Role) -> do_do(?fun_list, Role).

%% ----------------------------------------------------
%% 私有函数
%% ----------------------------------------------------

%% 执行调用
do_do([], Role) -> end_do(Role);
do_do([{M, F} | T], Role = #role{name = Name}) ->
    case catch erlang:apply(M, F, [Role]) of
        ok ->
            do_do(T, Role);
        {ok, NewRole} when is_record(NewRole, role) ->
            do_do(T, NewRole);
        Err ->
            ?ERR("角色[~ts]执行退出调用[~w:~w]时发生异常: ~w", [Name, M, F, Err]),
            do_do(T, Role)
    end.
-ifdef(enable_gm).
end_do(Role) ->
    case get(role_bak_flag) =:= true of
        true -> Role; %% get(role_bak);  %% 不存档处理 方便前期测试
        _ -> Role
    end.
-else.
end_do(Role) -> Role.
-endif.

