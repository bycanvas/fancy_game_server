%%----------------------------------------------------
%% 角色属性测试
%%----------------------------------------------------
-module(test_role_rpc).
-export([
        handle/3
    ]
).
-include("common.hrl").
-include("tester.hrl").

handle(role_init, [], #tester{id = {_Rid, _P, _Z}}) ->
    tester:pack_send(10000, {}),
    ?T_DBG("[~w, ~ts, ~w]请求获取角色信息", [_Rid, _P, _Z]),
    ok;

%% =========================================================================

handle(10000, {Rid, P, Z, _Name, Lev, HpMax, Hp}, T = #tester{id = {Rid, P, Z}}) ->
    ?T_DBG("[~w, ~ts, ~w]收到角色信息，开始进入地图", [Rid, P, Z]),
    tester:cmd(test_map_rpc, enter, []),
    {ok, T#tester{lev = Lev, hp_max = HpMax, hp = Hp}};

%% 容错处理
handle(_Cmd, _Data, #tester{name = _Name}) ->
    ?T_DBG("[~ts]收到未知消息[~w]: ~w", [_Name, _Cmd, _Data]),
    ok.
