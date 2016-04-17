%%----------------------------------------------------
%% 角色登录测试
%%----------------------------------------------------
-module(test_client_rpc).
-export([
        handle/3
    ]
).
-include("common.hrl").
-include("tester.hrl").

%% 请求角色列表
handle(role_list, [], _T) ->
    tester:pack_send(1101, {}),
    ok;

%% 创建角色
handle(create_role, [Name], _T) ->
    tester:pack_send(1102, {Name}),
    ok;

%% 登录指定角色
handle(login_role, [Rid, P, Z], _T) ->
    tester:pack_send(1103, {Rid, P, Z}),
    ok;

%% =========================================================================

handle(1101, {?true, _, []}, #tester{name = Name}) ->
    tester:cmd(test_client_rpc, create_role, [Name]),
    ok;
handle(1101, {?true, _, RoleList = [_|_]}, T) ->
    {Rid, P, Z, Name} = util:rand_list(RoleList),
    put(rid, Rid),
    tester:cmd(test_client_rpc, login_role, [Rid, P, Z]),
    {ok, T#tester{name = Name}};
handle(1101, {?false, _Msg, _}, #tester{name = _Name}) ->
    ?T_DBG("登录角色[~ts]失败:~ts", [_Name, _Msg]),
    stop;

handle(1102, {?false, _Msg, _, _, _}, #tester{name = _Name}) ->
    ?T_DBG("创建角色[~ts]失败:~ts", [_Name, _Msg]),
    stop;
handle(1102, {?true, _, Rid, P, Z}, T = #tester{name = _Name}) ->
    ?T_DBG("创建角色[~ts]成功", [_Name]),
    put(rid, Rid),
    {ok, T#tester{id = {Rid, P, Z}}};

handle(1103, {?false, _Msg}, #tester{platform = _P, zone_id = _Z}) ->
    ?T_DBG("登录角色[~w, ~ts, ~w]失败: ~ts", [get(rid), _P, _Z, _Msg]),
    ok;
handle(1103, {?true, _Msg}, T = #tester{platform = P, zone_id = Z}) ->
    Rid = get(rid),
    ?T_DBG("登录角色[~w, ~ts, ~w]成功", [Rid, P, Z]),
    self() ! login_role_done,
    {ok, T#tester{id = {Rid, P, Z}}};

handle(1199, {_Ts}, _T) ->
    ok;

%% 容错处理
handle(_Cmd, _Data, #tester{name = _Name}) ->
    ?T_DBG("[~ts]收到未知消息[~w]: ~w", [_Name, _Cmd, _Data]),
    ok.
