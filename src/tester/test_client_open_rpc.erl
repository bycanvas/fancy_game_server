%%----------------------------------------------------
%% 客户端连接测试
%%----------------------------------------------------
-module(test_client_open_rpc).
-export([
        handle/3
    ]
).
-include("common.hrl").
-include("tester.hrl").

handle(login, [Account, Platform, ZoneId], _T) ->
    SessionId = <<>>,
    tester:pack_send(1001, {Account, Platform, ZoneId, SessionId}),
    ok;

%% =========================================================================

handle(1001, {?false, _Msg}, _T = #tester{account = _Account, platform = _Platform, zone_id = _ZoneId}) ->
    ?ERR("帐号[~ts]登录区服[~ts, ~w]失败:~ts", [_Account, _Platform, _ZoneId, _Msg]),
    stop;
handle(1001, {?true, _Msg}, _T = #tester{account = _Account, platform = _Platform, zone_id = _ZoneId}) ->
    ?T_DBG("账号[~ts]登录区服[~ts, ~w]成功", [_Account, _Platform, _ZoneId]),
    %% self() ! connected_zone,
    ok;

%% 容错处理
handle(_Cmd, _Data, #tester{name = _Name}) ->
    ?T_DBG("[~ts]收到未知消息[~w]: ~w", [_Name, _Cmd, _Data]),
    ok.
