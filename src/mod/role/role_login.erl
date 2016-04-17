%%----------------------------------------------------
%% 角色登录时回调
%% 该模块回调函数运行于角色进程内
%%----------------------------------------------------
-module(role_login).
-export([
        do/1
    ]).
-include("common.hrl").
-include("role.hrl").
-include("timer.hrl").

%% 登陆需要调用的函数列表
%% 列表中的函数及返回格式要求如下:
%% fun(Role::#role{}) -> ok | {ok, NewRole::#role{}}.
-define(login_fun_list, [
        
    ]).



%% ----------------------------------------------------
%% 外部接口
%% ----------------------------------------------------

%% @doc 执行登录调用
-spec do(Role::#role{}) -> {ok, NewRole::#role{}} | {error, Reason::term()}.
do(Role = #role{name = <<>>}) ->
    %% role:send_buff_begin(),
    Result = do_do(?login_fun_list, role_data:role_init(Role)),
    %% role:send_buff_clean(),
    Result;
do(Role) ->
    %% role:send_buff_begin(),
    Result = do_do(?login_fun_list, Role),
    %% role:send_buff_clean(), %% 清除登陆处理中的推送消息
    Result.


%% ----------------------------------------------------
%% 私有函数
%% ----------------------------------------------------
%% 执行调用
do_do([], Role) -> {ok, Role};
do_do([{M, F} | T], Role = #role{name = Name}) ->
    case catch erlang:apply(M, F, [Role]) of
        ok ->
            do_do(T, Role);
        {ok, NewRole} when is_record(NewRole, role) ->
            do_do(T, NewRole);
        Err ->
            ?ERR("角色[~ts]执行调用[~w:~w]时发生异常: ~w", [Name, M, F, Err]),
            {error, Err}
    end.
