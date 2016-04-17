%%----------------------------------------------------
%% 数据库写入队列管理模块
%%----------------------------------------------------
-module(db_queue_mgr).
-behaviour(gen_server).
-export([
        start_link/0
    ]
).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
-record(state, {}).

-include("common.hrl").
-include("persist.hrl").

%% @doc 启动进程
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).


%%---------------------------------------
%% 内部方法
%%---------------------------------------
init([]) ->
    ?INFO("[~w] 正在启动...", [?MODULE]),
    process_flag(trap_exit, true),
    start_db_queue(lists:seq(1, ?DB_QUEUE_NUM)),
    State = #state{},
    ?INFO("[~w] 启动完成", [?MODULE]),
    {ok, State}.

handle_call(_Request, _From, State) ->
    {noreply, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ?INFO("[~w] 正在关闭...", [?MODULE]),
    ?INFO("[~w] 关闭完成", [?MODULE]),
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.


%%--------------------------------------
%% 数据加载
%%--------------------------------------
%% 启动持久化队列进程
start_db_queue([]) -> ok;
start_db_queue([Id|T])->
    Name = db_queue:db_queue_process_name(Id),
    Arg = {Name, {db_queue, start_link, [Id]}, transient, 60000, worker, [db_queue]},
    case supervisor:start_child(sup_db_queue, Arg) of
        {ok, _Pid} ->
            start_db_queue(T);
        _E ->
            ?ERR("开启持久化队列进程失败: ~w", [_E]),
            error
    end.
