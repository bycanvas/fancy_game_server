%%----------------------------------------------------
%% 测试器启动器
%%----------------------------------------------------
-module(test).
-behavior(gen_server).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
-export([
        start_link/0
        ,run/1,run/2
        ,count/0
        ,stat/0
        ,stop/0, stop/1
    ]).

-include("common.hrl").
-include("tester.hrl").

-define(test_info, {"127.0.0.1", 15001}).

-record(state, {max = 0}).

%% ----------------------------------------------------
%% 对外接口
%% ----------------------------------------------------
%% @doc 启动多个测试器
run(N) ->
    case env:get(last_run_test) of
        undefined -> run(1, N);
        M when N >= M -> run(M, N);
        M -> ?INFO("Error: run(~w) => run(~w,~w)", [N, M, N])
    end.

%% @doc 启动某个区间的多个测试器
run(N, M) ->
    util:for(N, M, fun(I) ->
                ?INFO("创建机器人，编号:~w", [I]),
                spawn(fun() ->
                            {Host, Port} = ?test_info,
                            case tester:start(I, Host, Port) of
                                {ok, _} -> ok;
                                {error, _Err} ->
                                    ?ERR("创建机器人[~w]失败:~w", [I, _Err]);
                                _ ->
                                    ok
                            end
                    end
                )
        end),
    env:set(last_run_test, M + 1).

%% @doc 统计人数
count() ->
    F = fun(#tester{id = 0}, {OnN, OffN, NoLoginN}) ->
            {OnN, OffN, NoLoginN + 1};
        (#tester{pid = Pid}, {OnN, OffN, NoLoginN}) ->
            case is_pid(Pid) andalso is_process_alive(Pid) of
                true -> {OnN + 1, OffN, NoLoginN};
                false -> {OnN, OffN + 1, NoLoginN}
            end
        end,
    {OnlineNum, OfflineNum, NoLoginNum} = ets:foldl(F, {0, 0, 0}, tester_online),
    ?INFO("测试在线人数:~w  掉线人数:~w 未登陆人数:~w", [OnlineNum, OfflineNum, NoLoginNum]),
    ok.

%% @doc 统计人数返回
stat() ->
    F = fun(#tester{pid = Pid}, {OnN, OffN}) ->
                case is_pid(Pid) andalso is_process_alive(Pid) of
                    true -> {OnN + 1, OffN};
                    false -> {OnN, OffN + 1}
                end
        end,
    {OnlineNum, OfflineNum} = ets:foldl(F, {0, 0}, tester_online),
    F1 = fun(#tester{id = 0}, Num) ->
                Num + 1;
           (_, Num) ->
                Num
        end,
    NoLoginNum = ets:foldl(F1, 0, tester_online),
    {OnlineNum, OfflineNum, NoLoginNum}.

%% @doc 重置测试ID(结束所有测试客户端)
stop() ->
    L = ets:tab2list(tester_online),
    F = fun(Pid) ->
            Pid ! logout
    end,
    [F(Pid) || #tester{pid = Pid} <- L],
    env:set(last_run_test, undefined),
    ok.

%% @doc 结束指定ID测试客户端
stop(RoleId) ->
    case ets:lookup(tester_online, RoleId) of
        [] -> ignore;
        [#tester{pid = Pid}] ->
            Pid ! logout
    end,
    ok.

%% @doc 启动
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).


%% ----------------------------------------------------
%% 内部处理
%% ----------------------------------------------------
init([]) ->
    ?INFO("[~w] 正在启动...", [?MODULE]),
    ets:new(tester_online, [set, named_table, public, {keypos, #tester.id}]),
    erlang:send_after(60*1000,self(),loop),
    State = #state{},
    ?INFO("[~w] 启动完成", [?MODULE]),
    {ok, State}.

handle_call(_Request, _From, State) ->
    {noreply, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(loop, State) ->
    erlang:send_after(60*1000,self(),loop),
    %% L = offline(),
    %% {Host, Port} = ?test_info,
    %% lists:foreach(fun({N, Acc}) ->
    %%             ?INFO("重启角色[~ts]", [Acc]),
    %%             util:sleep(100),
    %%             tester:start(N, Host, Port)
    %%     end, L),
    {noreply, State};

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% ----------------------------------------------------
%% 私有函数
%% ----------------------------------------------------
