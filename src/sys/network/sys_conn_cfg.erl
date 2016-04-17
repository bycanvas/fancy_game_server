%%----------------------------------------------------
%% 连接器配置
%% （为了sys_conn解耦，把耦合的东西写在这里）
%% （需要修改sys_conn的行为的，就在这里修改）
%%----------------------------------------------------
-module(sys_conn_cfg).
-export([
        get_init_jobs/0
        ,module/1
        ,module/2
    ]
).

-export([
        check_bind_obj/1
    ]
).
-include("common.hrl").
-include("conn.hrl").

%% ----------------------------------------------------
%% 对外接口
%% ----------------------------------------------------
%% @doc 初始化任务
-spec get_init_jobs() -> [#conn_job{}].
get_init_jobs() ->
    [
        #conn_job{label = check_bind_obj, interval = 150000, m = ?MODULE, f = check_bind_obj, a = []}
    ].

%% @doc 模块映射信息
%% <ul>
%% <li>Type: 模块类型game_server | monitor | tester</li>
%% <li>Cmd: 命令号，约定有效范围:100~65500，模块号有效范围:1~655</li>
%% <li>NeedAuth: 调用时是否需要先通过验证</li>
%% <li>Caller: 指定调用发起者</li>
%% <li>Parser: 协议解析模块</li>
%% <li>DecodeRec: 协议解析对应的PB的Record</li>
%% <li>ModName: 模块名</li>
%% <li>Reason: 返回出错的模块编号</li>
%% </ul>
-spec module(Type, Cmd) -> {ok, NeedAuth, Caller, Parser, DecodeRec, ModName} | {ok, Parser, ModName} | {error, Reason} when
    Type :: game_server | monitor | tester,
    Cmd :: pos_integer(),
    NeedAuth :: boolean(),
    Caller :: connector | object,
    Parser :: atom(),
    DecodeRec :: atom(),
    ModName :: atom(),
    Reason :: term().
module(Cmd) -> module(game_server, Cmd).
module(Type, Cmd) -> code(Type, trunc(Cmd / 100)).


%% ----------------------------------------------------
%% 私有函数
%% ----------------------------------------------------
%% 检查绑定对象
check_bind_obj(#conn{bind_obj = undefined}) -> stop;
check_bind_obj(#conn{bind_obj = BindObj}) when BindObj=/=undefined -> ok.

%% -------------------------------
%% 具体映射
%% -------------------------------
%% 游戏服
code(game_server, 10)      -> {ok, false, connector, proto_10, client_open_rpc};
code(game_server, 11)      -> {ok, false, connector, proto_11, client_rpc};
code(game_server, 100)     -> {ok, true, object, proto_100, role_rpc};
code(game_server, 101)     -> {ok, true, object, proto_101, map_rpc};

%% 测试器
code(tester, 10)           -> {ok, proto_10, test_client_open_rpc};
code(tester, 11)           -> {ok, proto_11, test_client_rpc};
code(tester, 100)           -> {ok, proto_100, test_role_rpc};
code(tester, 101)           -> {ok, proto_101, test_map_rpc};

%% 未知编号
code(Type, Code)           -> {error, {unknow_mapping, Type, Code}}.
