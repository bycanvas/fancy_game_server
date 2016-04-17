%%----------------------------------------------------
%% 启动器
%%----------------------------------------------------
-module(main).
-behaviour(application).
-export([
        start/0
        ,start/2
        ,stop/0
        ,stop/1
        ,stop_from_shell/0
        ,hotswap_from_shell/0
        ,exec_from_shell/0
    ]
).
-define(APPS, [crypto, emysql, main]).
-include("common.hrl").

%%----------------------------------------------------
%% 对外接口
%%----------------------------------------------------

%% @doc 启动系统
-spec start() -> ok.
start() ->
    start_applications(?APPS).

%% @doc 启动系统
-spec start(term(), list()) -> {ok, pid()} | {ok, pid(), term()} | {error, term()}.
start(_, _) ->
    io:setopts([{encoding, unicode}]),
    case init:get_plain_arguments() of
        [NodeType] -> sup_boot:start_link(list_to_atom(NodeType));
        _ -> {error, args_error}
    end.

%% @doc 从shell执行关机操作
-spec stop_from_shell() -> ok.
stop_from_shell() ->
    [Name] = init:get_plain_arguments(),
    Node = list_to_atom(Name),
    case net_adm:ping(Node) of
        pang -> ?INFO("无法访问节点[~w]", [Node]);
        pong ->
            case rpc:call(Node, main, stop, [], 300000) of
                ok -> ignore;
                Err -> ?ERR("关闭节点[~ts]时发生异常: ~w", [Name, Err])
            end
    end,
    util:sleep(1000),
    init:stop().

%% @doc 从shell执行热更新
-spec hotswap_from_shell() -> ok.
hotswap_from_shell() ->
    [M] = init:get_plain_arguments(),
    Node = list_to_atom(M),
    Args = case init:get_argument(hotswap_mods) of
        {ok, [["all"]]} -> [];
        {ok, [L]} -> [L];
        _ -> []
    end,
    case net_adm:ping(Node) of
        pang -> ?ERR("无法访问节点[~ts]，热更新操作失败", [M]);
        pong ->
            case rpc:call(Node, dev, u, Args, 120000) of
                ok -> ignore;
                Err -> ?ERR("热列新节点[~ts]时发生异常: ~w", [M, Err])
            end
    end,
    timer:sleep(1000),
    init:stop().

%% @doc 从shell执行代码
%% 格式：
%% N = s1@local.dev
%% T = [{M, F, A}, {M, F, A}]
exec_from_shell() ->
    [N|T] = init:get_plain_arguments(),
    Node = list_to_atom(N),
    case T of
        [] ->
            ?ERR("没有要执行的代码...");
        [_] ->
            case util:string_to_term(T) of
                {ok, MFAs} ->
                    case net_adm:ping(Node) of
                        pang -> ?ERR("无法访问节点[~w]，执行代码失败", [Node]);
                        pong ->
                            lists:foreach(fun({M, F, A}) ->
                                        case rpc:call(Node, M, F, A, 120000) of
                                            ok -> ignore;
                                            Err -> ?ERR("节点[~ts]执行[~w, ~w, ~w]时发生异常: ~w", [Node, M, F, A, Err])
                                        end
                                end, MFAs)
                    end;
                _Err ->
                    ?ERR("无法转化成可执行的代码:~w", [T])
            end;
        _ ->
            ?ERR("无法识别的可执行代码格式:~w", [T])
    end,
    util:sleep(1000),
    init:stop().

%% @doc 关闭服务器
-spec stop() -> ok | {error, term()}.
stop() ->
    ?INFO(">> 正在关闭节点[~w]...", [node()]),
    try
        stop_applications(?APPS),
        ?P(">> 节点[~w]已经关闭.", [node()])
    catch
        T:X ->
            ?INFO("节点[~w]关闭时发生异常[~w]: ~w", [node(), T, X]),
            {error, X}
    end,
    init:stop().

-spec stop(term()) -> ok.
stop(_State) ->
    ok.

%%----------------------------------------------------
%% 私有函数
%%----------------------------------------------------

%% @doc 按次序启动app
start_applications(Apps) ->
    manage_applications(
        fun lists:foldl/3,
        fun application:start/1,
        fun application:stop/1,
        already_started,
        cannot_start_application,
        Apps
    ).

%% @doc 按启动时相反的次序关闭app
stop_applications(Apps) ->
    manage_applications(
        fun lists:foldr/3,
        fun application:stop/1,
        fun application:start/1,
        not_started,
        cannot_stop_application,
        Apps
    ).

manage_applications(Iterate, Do, Undo, SkipError, ErrorTag, Apps) ->
    F = fun(App, Acc) ->
            case Do(App) of
                ok -> [App | Acc];
                {error, {SkipError, _}} -> Acc;
                {error, Reason} ->
                    lists:foreach(Undo, Acc),
                    throw({error, {ErrorTag, App, Reason}})
            end
    end,
    Iterate(F, [], Apps),
    ok.
