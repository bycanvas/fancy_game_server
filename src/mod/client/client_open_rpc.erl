%%----------------------------------------------------
%% 客户端验证
%% **注意**以下调用都是开放的，请注意安全性
%%----------------------------------------------------
-module(client_open_rpc).
-export([handle/3]).
-include("common.hrl").
-include("conn.hrl").
-include("role.hrl").


%% 请求验证账号登录
handle(1001, {Account, Platform, ZoneId, SessionId}, Conn = #conn{bind_obj = undefined}) ->
    case check_acc(Account, Platform, ZoneId, SessionId) of
        false ->
            {stop, reply, {?false, ?T("帐号验证失败，无法登录")}};
        true ->
            Acc = {Account, Platform, ZoneId},
            NewConn = Conn#conn{bind_obj = Acc},
            case role_data:fetch(by_acc, Acc) of
                {ok, #role{id = RoleId, name = Name}} ->
                    Rl = [{RoleId, Name}],
                    sys_conn:pack_send(self(), 1101, {?true, <<>>, Rl}),
                    {reply, {?true, <<>>}, NewConn};
                false ->
                    sys_conn:pack_send(self(), 1101, {?true, <<>>, []}),
                    {reply, {?true, <<>>}, NewConn};
                {error, _} ->
                    {stop, reply, {?false, ?T("获取角色列表失败，请稍后重试")}}
            end
    end;

%% 容错匹配
handle(_Cmd, _Data, _Conn) ->
    ?DEBUG("无效的RPC调用[~w]: ~w, ~w", [_Cmd, _Data, _Conn]),
    {stop, "错误的请求"}.

%% ----------------------------------------------------
%% 私有函数
%% ----------------------------------------------------
%% 检查帐号是否正确
check_acc(_Account, _Platform, _ZoneId, _SessionId) -> true.
