%%----------------------------------------------------
%% 开发工具
%%----------------------------------------------------
-module(dev).
-export([
        info/0
        ,mq_info/2
        ,mq_info/3
        ,mq_info_top/2
        ,top/0
        ,top/1
        ,top/3
        ,eprof_start/0
        ,eprof_start/2
        ,eprof_stop/0
        ,m/0
        ,m/1
        ,u/0
        ,u/1
        ,u/2
        ,u_db/1
        ,edoc/0
        ,get_all_erl/0
        ,file_list/2
    ]
).

-include_lib("kernel/include/file.hrl").
-include("common.hrl").

%% @doc 查看系统当前的综合信息
-spec info() -> ok.
info() ->
    SchedId      = erlang:system_info(scheduler_id),
    SchedNum     = erlang:system_info(schedulers),
    ProcCount    = erlang:system_info(process_count),
    ProcLimit    = erlang:system_info(process_limit),
    ProcMemUsed  = erlang:memory(processes_used),
    ProcMemAlloc = erlang:memory(processes),
    MemTot       = erlang:memory(total),
    OnlineNum    = ets:info(ets_role_login, size),
    RoleNum      = dets:info(role_data, size),
    {MaxFDsize,_}= string:to_integer(os:cmd("ulimit -n")),
    FDsize       = length(string:tokens(os:cmd("lsof -d \"0-9999999\" -lna -Ff -p " ++ os:getpid()), "\n")),
    {CoreSize,_} = string:to_integer(os:cmd("ulimit -c")),
    DistBufBusyLimit    = erlang:system_info(dist_buf_busy_limit),
    util:cn(
        "   当前节点名:                           ~p~n"
        "   Scheduler id:                         ~p~n"
        "   Num scheduler:                        ~p~n"
        "   Memory used by erlang processes:      ~p~n"
        "   Memory allocated by erlang processes: ~p~n"
        "   The total amount of memory allocated: ~p~n"
        "   可创建进程数量上限:                   ~p~n"
        "   当前节点进程数:                       ~p~n"
        "   本节点在线玩家数:                     ~p~n"
        "   本节点玩家总数:                       ~p~n"
        "   本节点文件描述符上限:                 ~p~n"
        "   本节点当前打开PORT数:                 ~p~n"
        "   本节点CoreDump文件大小上限:           ~p~n"
        "   本节点RPC通道缓存:                    ~p Bytes~n"
        , [node(), SchedId, SchedNum, ProcMemUsed, ProcMemAlloc, MemTot, ProcLimit, ProcCount, OnlineNum, RoleNum, MaxFDsize, FDsize, CoreSize, DistBufBusyLimit]),
    ok.

%% @doc 返回当前及诶单的占用资源最多的列表信息
-spec top() -> ProcList::list().
top() ->
    ?INFO("显示占用内存最多的10个进程:~n"),
    top(mem),
    ?INFO("显示消息队列最多的10个进程:~n"),
    top(queue),
    ?INFO("显示CPU占用最多的10个进程:~n"),
    top(reds),
    ?INFO("显示bin使用最多的10个进程:~n"),
    top(bin).

%% @doc 返回当前节点中占用资源最多的N个进程列表.
%% Type = mem | queue | reds
-spec top(Type::atom()) -> ProcList::list().
%% Type: 排名类型
%% <ul>
%%     <li>{@type mem} 返回当前节点中内存占用前N的进程</li>
%%     <li>{@type queue} 返回当前节点中消息队列长度前N的进程</li>
%%     <li>{@type reds} 返回当前节点中reductions值前N的进程</li>
%% </ul>
top(mem) ->
    top(mem, 1, 10);
top(queue) ->
    top(queue, 1, 10);
top(reds) ->
    top(reds, 1, 10);
top(bin) ->
    top(bin, 1, 10).

%% @doc 返回当前节点中占用资源最多的N个进程列表.
%% ProcList = list()
-spec top(Type::atom(), Start::integer(), Len::integer()) -> ProcList::list().
top(Type, Start, Len) ->
    TopType = if
        Type =:= mem -> memory;
        Type =:= queue -> message_queue_len;
        Type =:= reds -> reductions;
        Type =:= bin -> binary;
        true -> undefined
    end,
    L = do_top(TopType, Start, Len),
    util:cn(
        "~10s ~24s ~30s ~30s ~12s ~12s ~10s ~10s~n"
        , ["Pid", "registered_name", "initial_call", "current_call", "memory", "reductions", "msg_len", "bin"]
    ),
    print_top(lists:reverse(L)).

%% @doc 打印进程消息队列
-spec mq_info(PidStr, N, Order) -> ok when
    PidStr :: list(), %% 进程PID
    N :: integer(), %% 取消息队列前N个
    Order :: asc | desc.
mq_info(P, N) ->
    mq_info(P, N, asc).
mq_info(P, N, Order) when is_list(P) ->
    mq_info(list_to_pid(P), N, Order);
mq_info(P, N, Order) when node(P) == node() ->
    L = erlang:process_info(P),
    case lists:keyfind(messages, 1, L) of
        {_, Mgs} when is_list(Mgs) ->
            Mgs1 = case Order of
                desc -> lists:reverse(Mgs);
                _ -> Mgs
            end,
            Len = length(Mgs1),
            N1 = if
                N < 0 -> 0;
                N > Len -> Len;
                true -> N
            end,
            {L1, _} = lists:split(N1, Mgs1),
            ?INFO("Pid=~w的消息队列前N=~w个：~w", [P, N, L1]);
        _ -> ?INFO("错误的参数:P=~w, N=~w", [P, N])
    end.

%% @doc 打印消息队列最高的几个进程的消息队列
-spec mq_info_top(TopNum, MsgNum) -> ok when
    TopNum :: integer(), %% 消息队列最高的多少个
    MsgNum :: integer(). %% 消息条数
mq_info_top(TopNum, MsgNum) ->
    case dev:top(queue, 1, TopNum) of
        L when is_list(L) ->
            lists:foreach(fun(Args) ->
                        case Args of
                            [Pid, Name, _, Memory, Reds, MsgLen] ->
                                ?INFO("Pid=~w, Name=~ts, Memory=~w, Reds=~w, MsgLen=~w, 消息队列前[~w]个:", [Pid, Name, Memory, Reds, MsgLen, MsgNum]),
                                mq_info(Pid, MsgNum);
                            _ ->
                                ?INFO("top(queue)返回格式错误:~w", [Args])
                        end
                    end, L);
        _ -> ?INFO("top(queue)返回格式错误")
    end.

%% @doc 开始对当前进程执行eprof分析程序
-spec eprof_start() -> ok.
eprof_start() -> eprof_start(pid, [self()]).

%% @doc 开始执行eprof分析程序
-spec eprof_start(pid, Pids) -> ok when
    Pids :: [pid()].
eprof_start(pid, Pids) ->
    case eprof:start_profiling(Pids) of
        profiling -> ?INFO("eprof启动成功");
        E -> ?INFO("eprof启动失败: ~w", [E])
    end;
%% @doc 对指定角色执行eprof分析
eprof_start(role, {Rid, Platform, SrvId}) ->
    case role_query:pid({Rid, Platform, SrvId}) of
        {ok, Pid} -> eprof_start(pid, [Pid]);
        _ -> ?INFO("角色[Rid:~w Platform:~ts SrvId:~w]不在线", [Rid, Platform, SrvId])
    end;
%% @doc 对联盟进程执行eprof分析
eprof_start(clan, ClanId) ->
    case clan:pid(ClanId) of
        {ok, Pid} -> eprof_start(pid, [Pid]);
        {error, not_found} -> ?INFO("联盟[~w]不存在", [ClanId]);
        _ -> ?INFO("联盟[~w]不在线", [ClanId])
    end.

%% @spec eprof_stop() -> ok
%% @doc 停止eprof并输出分析结果到指eprof_analyze.log文件中
eprof_stop() -> eprof_stop("eprof_analyze").
%% @doc 停止eprof并输出分析结果到指定文件
-spec eprof_stop(FileName) -> ok when
    FileName :: string().
eprof_stop(FileName) ->
    File = FileName ++ ".log",
    eprof:stop_profiling(),
    eprof:log(File),
    eprof:analyze(total),
    ?INFO("eprof分析结果已经输出到了:~ts", [File]).

%% @doc 编译并热更新模块(生产模式)
-spec m() -> ok.
m() ->
    %% make(main, []).
    m(main).

%% @doc 编译并热更新模块(使用标准调试模式)
-spec m(Type::atom()) -> ok.
m(main) ->
    make(main, [{d, debug}, {d, dbg_socket}, {d, disable_auth}, {d, enable_gm}, {d, dbg_lag}]);
m(tester) ->
    make(tester, [{d, dbg_tester}, {d, disable_auth}, {d, enable_gm}]);
m(data) ->
    make(data, []).

%% @doc 编译并更新模块
%% 有效编译参数:
%% <ul>
%% <li>debug            开启debug模式，打开宏?DEBUG的输出</li>
%% <li>dbg_sql          开启数据库调试模式，打印所有的SQL查询信息</li>
%% <li>dbg_socket       开启socket调试模式，打印所有收发的socket数据</li>
%% <li>dbg_lag          开启网络延时模拟，波动延时为100~300</li>
%% <li>enable_gm_cmd    开启GM命令</li>
%% <li>disable_auth     关闭ticket验证</li>
%% </ul>
-spec make(atom() | string(), list()) -> ok.
make(main, Param) ->
    make(env:get(code_path), Param);
make(data, Param) ->
    make(env:get(code_path) ++ "/data", Param);
make(tester, Param) ->
    make(env:get(code_path) ++ "/tester", Param);
make(Path, Param) ->
    util:cn("### 正在编译(~ts)，参数:~w~n", [Path, Param]),
    file:set_cwd(Path),
    case make:all(Param) of
        up_to_date -> do_up([], false);
        _ -> ignore
    end,
    file:set_cwd(env:get(zone_path)). %% 返回节点目录

%% @doc 热更新所有模块(非强制)
-spec u() -> ok.
u() ->
    do_up([], false),
    ok.

%% @doc 热更新所有模块(强制更新)
%% <ul>
%% <li>force 强制更新所有模块</li>
%% <li>[atom()] 非强制更新指定模块</li>
%% </ul>
-spec u(Options) -> ok when
    Options :: force | [atom()].
u(force) ->
    do_up([], true);
u(ModList) when is_list(ModList) ->
    do_up(ModList, false).

%% @doc 热更新指定模块(强制更新)
-spec u([atom()], F::force) -> ok.
u(ModList, force) when is_list(ModList) ->
    do_up(ModList, true).

%% @doc 热更新数据库语句
%% <div>不可逆</div>
-spec u_db(DbSql::string()) -> ok | error.
u_db(DbSql) ->
    util:cn("--- 正在热更新节点SQL: ~w ---~n", [node()]),
    case db:exec(DbSql) of
        {error, _Why} ->
            util:cn(">> 热更新节点SQL失败: ~w", [_Why]),
            error;
        {ok, _} -> ok
    end.

%% @doc 生成API文档，将源码文件全部放入src同级的doc/目录中
-spec edoc() -> ok.
edoc() ->
    %% edoc:application(main, "./", []),
    {ok, Path} = file:get_cwd(),
    CodePath = Path ++ "/src",
    case file_list(CodePath, ".erl") of
        {error, _Why} -> ignore;
        {ok, L} ->
            edoc:files(do_edoc(L, []), [{new, true}, {dir, Path ++ "/doc"}])
    end,
    ok.

%% @doc 获取所有*.erl文件
%% <div>注意：对于没有访问权限的文件将不在出现在此列表中</div>
-spec get_all_erl() -> FileList | {error, term()} when
    FileList :: [{FileName, FilePath}],
    FileName :: string(),
    FilePath :: string().
get_all_erl() ->
    {ok, Cwd} = file:get_cwd(),
    Dir = Cwd ++ "src",
    case file:list_dir(Dir) of
        {ok, L} ->
            {ok, file_filter(L, Dir, ".erl", [])};
        _Other ->
            {error, _Other}
    end.

%% @doc 获取指定目录下指定类型的文件(包括子目录)
-spec file_list(Dir, Ext) -> {ok, FileList} | {error, Why} when
    Dir :: string(),
    Ext :: string(),
    FileList :: [{FilePath, FileName}],
    Why :: term(),
    FilePath :: string(),
    FileName :: string().
file_list(Dir, Ext) ->
    ?DEBUG("搜索目录[~ts]下的所有\"~ts\"文件", [Dir, Ext]),
    case file:list_dir(Dir) of
        {error, Reason} -> {error, Reason};
        {ok, L} -> {ok, file_filter(L, Dir, Ext, [])}
    end.

%% ----------------------------------------------------
%% 私有函数
%% ----------------------------------------------------

%% 执行更新
do_up(L, F) ->
    util:cn("--- 正在热更新节点: ~w ---~n", [node()]),
    Args = case {L, F} of
        {[], false}         -> [];
        {[], true}          -> [force];
        {[_H | _T], false}  -> [L];
        {[_H | _T], true}   -> [L, force]
    end,
    print_up(apply(sys_code, up, Args)).

%% 显示更新结果
print_up([]) -> ?P("~n");
print_up([{M, ok} | T]) ->
    util:cn("# 加载模块成功: ~p~n", [M]),
    print_up(T);
print_up([{M, {error, Reason}} | T]) ->
    util:cn("* 加载模块失败[~p]: ~p~n", [M, Reason]),
    print_up(T).

%% 格式化打钱top信息
print_top([]) -> ok;
print_top([H | T]) ->
    io:format("~10w ~24w ~30w ~30w ~12w ~12w ~10w ~10w~n", H),
    print_top(T).

%% 处理edoc使用的文件列表，过滤掉没有必要生成文档的文件
do_edoc([], L) -> L;
do_edoc([{M, F} | T], L) ->
    case util:text_banned(M, ["proto_.*", ".*_data", "sup_.*", ".*_rpc", "mysql.*"]) of
        true -> do_edoc(T, L);
        false -> do_edoc(T, [F | L])
    end.

%% 文件过滤，查找指定目录下的所有文件(包括子目录)，返回指定扩展名的文件列表
file_filter([], _Dir, _Ext, List) -> List;
file_filter([H | T], Dir, Ext, List) ->
    F = Dir ++ "/" ++ H,
    NewList = case file:read_file_info(F) of
        {ok, I} ->
            if
                I#file_info.type =:= directory ->
                    case file:list_dir(F) of
                        {ok, L} ->
                            D = Dir ++ "/" ++ H,
                            List ++ file_filter(L, D, Ext, []);
                        _Err ->
                            io:format("error list in directory:~p~n", [_Err]),
                            List
                    end;
                I#file_info.type =:= regular ->
                    case filename:extension(F) =:= Ext of
                        true ->
                            List ++ [{filename:basename(filename:rootname(F)), F}];
                        false ->
                            List
                    end;
                true ->
                    List
            end;
        _Other ->
            io:format("error in read_file_info:~p ~w~n", [F, _Other]),
            List
    end,
    file_filter(T, Dir, Ext, NewList).

%% top辅助函数
do_top(Type, Start, Len) when is_integer(Start), is_integer(Len), Start > 0, Len > 0 ->
    L = do_top1(Type, erlang:processes(), []),
    NL = lists:sublist(L, Start, Len),
    top_detail(NL, []).
do_top1(_Type, [], L) ->
    lists:sort(fun top_sort/2, L);
do_top1(Type, [P | Pids], L) ->
    NL = case process_info(P, Type) of
        {_, V} when Type =:= binary ->
            SortedBins = lists:usort(V),
            {_, SL, _} = lists:unzip3(SortedBins),
            [{P, lists:sum(SL)} | L];
        {_, V} -> [{P, V} | L];
        _ -> L
    end,
    do_top1(Type, Pids, NL).
top_detail([], L) -> L;
top_detail([{P, _} | T], L) ->
    Mem = case process_info(P, memory) of
        {_, V1} -> V1;
        _ -> undefined
    end,
    Reds = case process_info(P, reductions) of
        {_, V2} -> V2;
        _ -> undefined
    end,
    InitCall = case process_info(P, initial_call) of
        {_, V3} -> V3;
        _ -> undefined
    end,
    CurrCall = case process_info(P, current_function) of
        {_, V4} -> V4;
        _ -> undefined
    end,
    MsgLen = case process_info(P, message_queue_len) of
        {_, V5} -> V5;
        _ -> undefined
    end,
    RegName = case process_info(P, registered_name) of
        {_, V6} -> V6;
        _ -> undefined
    end,
    Bin = case process_info(P, binary) of
        {_, V7}  ->
            SortedBins = lists:usort(V7),
            {_, SL, _} = lists:unzip3(SortedBins),
            lists:sum(SL);
        _ -> undefined
    end,
    top_detail(T, [[P, RegName, InitCall, CurrCall, Mem, Reds, MsgLen, Bin] | L]).
top_sort({_, A}, {_, B}) when A =< B -> false;
top_sort(_, _) -> true.
