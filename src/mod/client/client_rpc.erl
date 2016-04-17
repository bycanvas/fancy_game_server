%%----------------------------------------------------
%% 客户端登录处理
%%----------------------------------------------------
-module(client_rpc).
-export([
        handle/3
    ]).
-include("common.hrl").
-include("role.hrl").
-include("conn.hrl").
-include("link.hrl").

-define(HEARTBEAT_PERIOD, 30).          %% 心跳包周期，单位：秒（这个要改的话要和客户端说好）
-define(HEARTBEAT_ENDURE_TIME, 10).     %% 心跳包误差可接受时间范围，单位：秒
-define(HEARTBEAT_ENDURE_NUM, 6).       %% 心跳包误差可接受次数范围
-define(ANTI_ACC_CHEATER_FLAG, false).  %% 反加速挂开启选项


%% 查询玩家角色列表
handle(1101, _, #conn{bind_obj = undefined}) ->
    {stop, reply, {?false, ?T("您的帐号尚未登录"), []}};
handle(1101, _, #conn{bind_obj = {Account, Platform, ZoneId}}) ->
    case role_data:fetch(by_acc, {Account, Platform, ZoneId}) of
        {ok, #role{id = RoleId, name = Name}} ->
            Rl = [{RoleId, Name}],
            {reply, {?true, <<>>, Rl}};
        false ->
            {reply, {?true, <<>>, []}};
        {error, Reason} ->
            {stop, reply, {?false, Reason, []}}
    end;

%% 创建玩家数据
handle(1102, _, #conn{bind_obj = undefined}) ->
    {stop, reply, {?false, ?T("您的帐号尚未登录"), {0, <<>>, 0}}};
handle(1102, {Name}, Conn = #conn{bind_obj = {Account, Platform, ZoneId}, ip = ClientIp, socket = Socket, port = Port}) ->
    case create_role(Account, Platform, ZoneId, Name) of
        {false, Msg} ->
            %% 不断开，允许重复创建职业和性别
            %% 此阶段断开连接时，前端走直接重连的
            {reply, {?false, Msg, {0, <<>>, 0}}};
        {ok, #role{id = RoleId}} ->
            Link = #link{
                socket = Socket
                ,conn_pid = self()
                ,ip = ClientIp
                ,port = Port
            },
            case role_login(RoleId, Link, Conn) of
                {ok, Reply, NewConn} ->
                    sys_conn:pack_send(self(), 1103, Reply),
                    {reply, {?true, <<>>, RoleId}, NewConn};
                {false, {Result, Msg}} ->
                    {stop, reply, {Result, Msg, {0, <<>>, 0}}}
            end
    end;

%% 登录角色数据(在合服后可以登录不同区的玩家数据)
handle(1103, _, #conn{bind_obj = undefined}) ->
    {stop, reply, {?false, ?T("您的帐号尚未登录")}};
handle(1103, _, #conn{bind_pid = Pid}) when Pid =/= undefined ->
    {reply, {?false, ?T("您已经在线，不能重复登录")}};
handle(1103, {Rid, Platform, ZoneId}, Conn = #conn{bind_obj = {_, _, _}, socket = Socket, ip = Ip, port = Port}) ->
    ?DEBUG("玩家登录角色ID[~w, ~ts, ~w]", [Rid, Platform, ZoneId]),
    Link = #link{
        socket = Socket
        ,conn_pid = self()
        ,ip = Ip
        ,port = Port
    },
    RoleId = {Rid, Platform, ZoneId},
    case role_login(RoleId, Link, Conn) of
        {ok, Reply, NewConn} ->
            {reply, Reply, NewConn};
        {false, {Result, Msg}} ->
            {stop, reply, {Result, Msg}}
    end;

%% 接收心跳包(前段主动，间隔30~40s)
handle(1199, _, _Conn = #conn{}) ->
    {reply, {date:unixtime()}};

%% 容错匹配
handle(_Cmd, _Data, _Conn) ->
    ?DEBUG("模块[~w]收到无效的RPC调用[~w]: ~w", [?MODULE, _Cmd, _Data]),
    {error, unknow_command}.

%% ----------------------------------------------------
%% 私有函数
%% ----------------------------------------------------
%% 创建玩家角色 -> {ok, #role{}} | {false, bitstring()}
create_role(Account, Platform, ZoneId, Name) ->
    Role = #role{
        id = {0, Platform, ZoneId}, account = Account
        ,name = Name, lev = 1
    },
    case role_data:create(Role) of
        {error, exist} ->
            {false, ?T("创建玩家数据失败：该账号数据已创建")};
        {error, zone_id_error} ->
            ?ERR("创建玩家数据失败: 分区请求错误~w", [ZoneId]),
            {false, ?T("创建玩家数据失败：您的分区信息错误")};
        {error, _Reason} ->
            ?ERR("创建玩家数据失败: ~w", [_Reason]),
            {false, ?T("创建玩家数据失败，请稍后再尝试创建")};
        {ok, NewRole} ->
            ?DEBUG("创建玩家数据成功: ~w", [NewRole#role.id]),
            {ok, NewRole}
    end.

%% 玩家数据登录 -> {false, Reply} | {ok, Reply, #conn{}}
role_login(RoleId = {Rid, Platform, ZoneId}, Link, Conn = #conn{bind_obj = {Account, _, _}}) ->
    case role_mgr:role_login(RoleId, Account, Link) of
        {error, {already_started, _}} ->
            ?ERR("玩家[Rid:~w, Platform:~ts, ZoneId:~w]登录失败: 玩家已经在线", [Rid, Platform, ZoneId]),
            {false, {?false, ?T("账号已经在线，不能重复登录")}};
        {error, not_exist} ->
            ?ERR("无法加载不存在的玩家数据[Account:~ts, Rid:~w, Platform:~ts ZoneId:~w]", [Account, Rid, Platform, ZoneId]),
            {false, {?false, ?T("无法加载不存在的玩家")}};
        {error, Err} ->
            ?ERR("加载数据时发生异常: ~w", [Err]),
            {false, {?false, ?T("登陆加载数据时发生异常")}};
        {ok, RolePid} ->
            erlang:link(RolePid),
            {ok, {?true, <<>>}, Conn#conn{object = role, bind_pid = RolePid}}
    end.
