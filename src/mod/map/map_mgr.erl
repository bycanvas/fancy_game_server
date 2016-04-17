%%----------------------------------------------------
%% 地图管理器
%%----------------------------------------------------
-module(map_mgr).
-behaviour(gen_server).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
-export([
        start_link/0
        ,name/1
        ,pid/1
        ,info/1
        ,create/1
        ,sync/1
    ]
).
-include("common.hrl").
-include("map.hrl").

-record(state, {
        next_id = 100000    %% 下个动态地图ID，小于100000的为普通地图
    }
).

%% ----------------------------------------------------
%% 外部接口
%% ----------------------------------------------------

%% @doc 启动
-spec start_link() -> {ok, pid()} | ignore | {error, term()}.
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

%% @doc 动态创建地图
-spec create(pos_integer()) -> {ok, pid(), pos_integer()} | {error, term()}.
create(BaseId) ->
    NormalMap = map_data:startup(),
    case lists:member(BaseId, NormalMap) of
        true -> {error, map_type_error};
        false ->
            case map_data:get(BaseId) of
                {error, Reason} -> {error, Reason};
                {ok, #map_data{width = W, height = H}} ->
                    Id = gen_server:call(?MODULE, fetch_id),
                    Map = #map{id = Id, base_id = BaseId, width = W, height = H},
                    case catch map:start_link(Map) of
                        {ok, MapPid} -> {ok, MapPid, Id};
                        _Err ->
                            ?ERR("创建地图进程失败:~w", [_Err]),
                            {error, map_create_error}
                    end
            end
    end.

%% @doc 获取地图名称
name(MapBaseId) ->
    case map_data:get(MapBaseId) of
        {ok, #map_data{name = Name}} -> Name;
        _ -> <<"未知地图"/utf8>>
    end.

%% @doc 获取本服地图进程pid
-spec pid(MapId::pos_integer()) -> {ok, pid()} | {error, not_found}.
pid(MapId) ->
    case ets:lookup_element(map_info, MapId, #map.pid) of
        Pid when is_pid(Pid) -> {ok, Pid};
        _Err ->
            ?ERR("未发现地图进程[~w:~w]",[MapId, _Err]),
            {error, not_found}
    end.

%% @doc 获取本节点地图信息
-spec info(pos_integer()) -> {ok, #map{}} | {error, not_found}.
info(MapId) ->
    case catch ets:lookup(map_info, MapId) of
        [Map] -> {ok, Map};
        [] -> {error, not_found};
        _Other ->
            ?ERR("查找地图信息[~w]失败:~w", [MapId, _Other]),
            {error, not_found}
    end.

%% @doc 同步地图信息到ets中
-spec sync(#map{}) -> ok.
sync(Map) ->
    ets:insert(map_info, Map).


%% ----------------------------------------------------
%% 内部处理
%% ----------------------------------------------------

init([]) ->
    ?INFO("[~w] 正在启动...", [?MODULE]),
    process_flag(trap_exit, true),
    ets:new(map_info, [set, named_table, public, {keypos, #map.id}]),
    create_startup(map_data:startup()),
    State = #state{},
    ?INFO("[~w] 启动完成", [?MODULE]),
    {ok, State}.

%% 申请一个地图ID
handle_call(fetch_id, _From, State = #state{next_id = Id}) ->
    {reply, Id, State#state{next_id = Id + 1}};

handle_call(_Request, _From, State) ->
    {noreply, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

%% 处理地图进程正常退出
handle_info({'EXIT', _Pid, normal}, State) ->
    ?DEBUG("连接的进程[~w]已经正常退出", [_Pid]),
    {noreply, State};

%% 处理地图进程异常退出
handle_info({'EXIT', Pid, Why}, State) ->
    ?ERR("连接的进程[~w]异常退出: ~w", [Pid, Why]),
    {noreply, State};

handle_info(_Info, State) ->
    ?ERR("收到未知消息: ~w", [_Info]),
    {noreply, State}.

terminate(_Reason, _State) ->
    ?INFO("[~w] 正在关闭...", [?MODULE]),
    stop_all_map(),
    ?INFO("[~w] 关闭完成", [?MODULE]),
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% ----------------------------------------------------
%% 私有函数
%% ----------------------------------------------------
%% 启动固定地图
create_startup([]) -> ok;
create_startup([BaseId | T]) when BaseId < 100000 ->
    case map_data:get(BaseId) of
        error -> ?ERR("创建地图失败:不存在的地图[~w]", [BaseId]);
        #map_data{id = BaseId, width = W, height = H} ->
            Map = #map{id = BaseId, base_id = BaseId, width = W, height = H},
            case map:start_link(Map) of
                {ok, _Pid} -> ok;
                _Err -> ?ERR("创建地图失败:~w", [_Err])
            end
    end,
    create_startup(T);
create_startup([BaseId | T]) ->
    ?ERR("创建地图失败:固定地图的ID[~w]不能大于100000", [BaseId]),
    create_startup(T).

%% 关闭所有地图
stop_all_map() ->
    L = ets:tab2list(map_info),
    lists:foreach(fun(#map{pid = Pid}) ->
                map:sync_stop(Pid)
        end, L).
