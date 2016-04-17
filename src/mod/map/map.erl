%%----------------------------------------------------
%% 地图进程
%%----------------------------------------------------
-module(map).
-behaviour(gen_server).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
-export([
        start_link/1
        ,stop/1, sync_stop/1
        ,pack_send_to_all/3
        ,role_enter/2, role_leave/1
        ,role_move/5, role_move_update_pos/5
        ,role_update/1, role_update_conn_pid/3
        ,async_leave/1
        ,is_blocked/4
    ]
).
-include("common.hrl").
-include("role.hrl").
-include("map.hrl").
-include("pos.hrl").
-include("link.hrl").

-define(MAP_CAST_MOVE_INTERVAL, 25).    %% 广播移动信息的间隔时间（单位：毫秒）


%% ----------------------------------------------------
%% 外部接口
%% ----------------------------------------------------

%% @doc 启动
-spec start_link(#map{}) -> {ok, pid()} | ignore | {error, term()}.
start_link(Map) ->
    gen_server:start_link(?MODULE, [Map], []).

%% @doc 关闭地图
-spec stop(pid()) -> ok.
stop(MapPid) when is_pid(MapPid) ->
    MapPid ! stop,
    ok;
stop(_) -> ok.

%% @doc 关闭地图（关机时使用）
sync_stop(MapPid) when is_pid(MapPid) ->
    ?CALL(MapPid, stop, infinity);
sync_stop(_) -> ok.

%% @doc 地图广播
-spec pack_send_to_all(MapId::pos_integer(), Cmd::pos_integer(), Data::tuple()) -> ok | {error, term()}.
pack_send_to_all(MapId, Code, Data) ->
    case sys_conn:pack(Code, Data) of
        {ok, Bin} ->
            send_to_all(MapId, Bin);
        Err ->
            ?ERR("打包地图广播数据出错[Code:~w][Err:~w]", [Code, Err]),
            {error, pack_error}
    end.

%% @doc 发送给地图广播数据
-spec send_to_all(MapInfo::pos_integer() | pid() | {bitstring(), pos_integer(), pos_integer()}, Bin::any()) -> ok | {error, term()}.
send_to_all(MapPid, Bin) when is_pid(MapPid) ->
    MapPid ! {send_to_all, Bin},
    ok;
send_to_all({P, Z, MapId}, Bin) ->
    case map_mgr:pid(P, Z, MapId) of
        {ok, MapPid} ->
            MapPid ! {send_to_all, Bin},
            ok;
        _Err ->
            ?ERR("~w",[_Err]),
            {error, map_id_error} 
    end;
send_to_all(MapId, Bin) ->
    case map_mgr:pid(MapId) of
        {ok, MapPid} ->
            MapPid ! {send_to_all, Bin},
            ok;
        _Err ->
            ?ERR("~w",[_Err]),
            {error, map_id_error} 
    end.

%% @doc 进入本服节点或跨服节点的指定地图
-spec role_enter(Entry, #role{}) -> {ok, NewRole} | {error, bitstring()} when
    NewRole :: #role{},
    Entry :: {MapId, DestX, DestY} | {Platform, ZoneId, MapId, DestX, DestY} | {Platform, ZoneId, MapId, MapGridPid, DestX, DestY},
    Platform :: bitstring(),
    ZoneId :: non_neg_integer(),
    MapGridPid :: non_neg_integer(),
    MapId :: pos_integer(),
    DestX :: non_neg_integer(), %% X轴小格子坐标
    DestY :: non_neg_integer(). %% Y轴小格子坐标
role_enter({MapId, DestX, DestY}, Role) ->
    case map_mgr:info(MapId) of
        {error, not_found} -> {error, ?T("地图不存在，无法进入")};
        {ok, Map} -> do_role_enter(DestX, DestY, Map, Role) 
    end.

do_role_enter(DestX, DestY, #map{id = _MapId, base_id = _BaseId, width = W, height = H}, _Role) when DestX < 0 orelse DestY < 0 orelse DestX > W orelse DestY > H ->
    ?ERR("进入地图[~w, ~w]时坐标错误:~w, ~w, ~w, ~w", [_MapId, _BaseId, W, H, DestX, DestY]),
    {error, ?T("地图坐标错误")};
do_role_enter(DestX, DestY, #map{id = MapId, base_id = BaseId, pid = MapPid}, Role) ->
    case role_leave(Role) of
        {ok, Role1 = #role{pos = Pos}} ->
            Pos1 = Pos#pos{map_id = MapId, map_base_id = BaseId, map_pid = MapPid, x = DestX, y = DestY},
            Role2 = Role1#role{pos = Pos1},
            {ok, MapRole} = role_convert:to(map_role, Role2),
            case ?CALL(MapPid, {role_enter, MapRole}, infinity) of
                ok ->
                    {ok, Role2};
                {false, Msg} ->
                    {error, Msg};
                _Err ->
                    ?ERR("角色[~ts]请求进入地图失败:~w", [_Err]),
                    {error, ?T("进入失败，无法进入地图")}
            end;
        {error, Msg} ->
            {error, Msg}
    end.

%% @doc 同步离开地图
-spec role_leave(#role{}) -> {ok, #role{}}.
role_leave(Role = #role{pos = undefined}) -> {ok, Role#role{pos = #pos{}}};
role_leave(Role = #role{pos = #pos{map_id = 0}})-> {ok, Role};
role_leave(Role = #role{pos = #pos{map_pid = undefined}}) -> {ok, Role}; %% 上线登录
role_leave(Role = #role{id = RoleId, name = _Name, pos = #pos{last = Last, map_id = MapId, map_base_id = BaseId, map_pid = MapPid}}) ->
    case ?CALL(MapPid, {role_leave, RoleId}, infinity) of
        ok ->
            Role1 = Role#role{pos = #pos{map_id = 0, last = Last}},
            {ok, Role1};
        {false, Msg} ->
            {error, Msg};
        _Err ->
            ?ERR("角色[~ts]离开地图[~w]失败:~w", [_Name, {MapId, BaseId}, _Err]),
            {error, ?T("离开地图失败")}
    end.

%% @doc 异步离开地图
-spec async_leave(#role{}) -> ok. %% {ok, #role{}}.
async_leave(#role{id = RoleId, pos = #pos{last = _Last, map_pid = MapPid}}) when is_pid(MapPid) ->
    MapPid ! {role_leave, RoleId},
    ok;
async_leave(_) -> ok.

%% @doc 角色移动
-spec role_move(BaseId::pos_integer(), Dx::non_neg_integer(), Dy::non_neg_integer(), Dir::non_neg_integer(), #role{}) -> ok.
role_move(MapId, DestX, DestY, Dir, #role{id = RoleId, pos = #pos{map_pid = MapPid}}) when is_pid(MapPid) ->
    MapPid ! {role_move, RoleId, MapId, DestX, DestY, Dir},
    ok;
role_move(_MapId, _DestX, _DestY, _Dir, _Role) ->
    ?ERR("请求角色移动错误忽略，不是同一个地图:~w", [_MapId]),
    ok.

%% @doc 角色移动同步位置
-spec role_move_update_pos(BaseId::pos_integer(), Dx::non_neg_integer(), Dy::non_neg_integer(), Dir::non_neg_integer(), #role{}) -> ok.
role_move_update_pos(MapId, DestX, DestY, Dir, #role{id = RoleId, pos = #pos{map_pid = MapPid}}) when is_pid(MapPid) ->
    MapPid ! {role_move_update_pos, RoleId, MapId, DestX, DestY, Dir},
    ok;
role_move_update_pos(_MapId, _DestX, _DestY, _Dir, _Role) ->
    ?ERR("请求角色移动同步错误忽略，不是同一个地图:~w", [_MapId]),
    ok.

%% @doc 角色刷新属性
-spec role_update(#role{}) -> ok.
role_update(Role = #role{pos = #pos{map_pid = MapPid}}) when is_pid(MapPid) ->
    case role_convert:to(map_role, Role) of
        {ok, Mr} ->
            MapPid ! {role_update, Mr};
        {error, Reason} ->
            ?ERR("角色转换失败: ~w", [Reason])
    end,
    ok;
role_update(#role{name = _Name}) ->
    ?DEBUG("角色[~ts]没有在地图中，忽略掉该刷新请求", [_Name]),
    ok.

%% @doc 更新缓存的conn_pid
-spec role_update_conn_pid(pid(), pid(), pid()) -> ok.
role_update_conn_pid(MapPid, RoleId, ConnPid) when is_pid(MapPid) ->
    MapPid ! {role_update_conn_pid, RoleId, ConnPid},
    ok;
role_update_conn_pid(_, _, _) ->
    ok.

%% @doc 是否不可行走区域
-spec is_blocked(MapId :: pos_integer(), MapBaseId :: pos_integer(), X :: pos_integer(), Y :: pos_integer()) -> boolean().
is_blocked(_MapId, _MapBaseId, _X, _Y) -> false.


%% ----------------------------------------------------
%% 内部处理
%% ----------------------------------------------------
init([Map = #map{id = MapId, base_id = MapBaseId, width = W, height = H}]) ->
    erlang:register(list_to_atom(lists:concat(["map_", MapId, "_", MapBaseId])), self()),
    process_flag(trap_exit, true),
    process_flag(priority, high),
    put(role_conn, []),
    put(role_move, []),
    NewMap = Map#map{pid = self(), width = W, height = H},
    init_grids(NewMap),
    map_mgr:sync(NewMap),
    erlang:send_after(?MAP_CAST_MOVE_INTERVAL, self(), cast_move),
    {ok, NewMap}.

%% 角色同步进入地图
handle_call({role_enter, MapRole = #map_role{id = RoleId, name = _Name}}, _From, Map = #map{id = _MapId, base_id = _MapBaseId}) ->
    case get({role, RoleId}) of
        undefined ->
            do_enter(Map, MapRole),
            {reply, ok, Map};
        _ ->
            ?DEBUG("角色[~w, ~ts]已经在地图[~w, ~w]里面，无需重复进入", [RoleId, _Name, _MapId, _MapBaseId]),
            {reply, ok, Map}
    end;

%% 角色同步离开地图
handle_call({role_leave, RoleId}, _From, Map) ->
    do_leave(Map, RoleId),
    {reply, ok, Map};

%% 关闭地图
handle_call(stop, _From, Map) ->
    {stop, normal, Map};

handle_call(_Request, _From, Map) ->
    {noreply, Map}.

handle_cast(_Msg, Map) ->
    {noreply, Map}.

%% 角色移动
handle_info({role_move, RoleId, MapId, DestX, DestY, Dir}, Map = #map{id = MapId, width = W, height = H}) ->
    case get({role, RoleId}) of
        #map_role{x = SrcX, y = SrcY, gx = SrcGx, gy = SrcGy} ->
            DestX1 = ?DX(DestX, W),
            DestY1 = ?DY(DestY, H),
            Move = #map_role_move{type = ?MAP_ROLE_MOVE_TYPE_MOVE, args = {RoleId, SrcX, SrcY, DestX1, DestY1, Dir, SrcGx, SrcGy}},
            put({last_move, RoleId}, Move),
            put(role_move, [Move | get(role_move)]);
        _ ->
            ?DEBUG("角色[~w]移动处理失败，该角色不在地图[~w]中", [RoleId, MapId])
    end,
    {noreply, Map};
handle_info({role_move, _RoleId, _MapId, _DestX, _DestY, _Dir}, Map) ->
    ?ERR("角色[~w]移动处理忽略，不匹配的地图ID:~w", [_RoleId, _MapId]),
    {noreply, Map};

%% 角色移动同步位置
handle_info({role_move_update_pos, RoleId, MapId, DestX, DestY, Dir}, Map = #map{id = MapId, width = W, height = H}) ->
    case get({role, RoleId}) of
        Mr = #map_role{pid = RolePid, x = SrcX, y = SrcY, gx = SrcGx, gy = SrcGy} ->
            DestX1 = ?DX(DestX, W),
            DestY1 = ?DY(DestY, H),
            DestGx1 = ?GX(DestX1),
            DestGy1 = ?GY(DestY1),
            Mr1 = Mr#map_role{x = DestX1, y = DestY1, dir = Dir, gx = DestGx1, gy = DestGy1},
            put({role, RoleId}, Mr1),
            RolePid ! {update_pos, DestX1, DestY1, Dir},
            do_move_update_pos(Mr1, {SrcX, SrcY}, {DestX1, DestY1}, {SrcGx, SrcGy}, {DestGx1, DestGy1});
        _ ->
            ?DEBUG("角色[~w]移动同步位置失败，该角色不在地图[~w]中", [RoleId, MapId])
    end,
    {noreply, Map};
handle_info({role_move_update_pos, _RoleId, _MapId, _DestX, _DestY, _Dir}, Map) ->
    ?ERR("角色[~w]移动同步位置处理忽略，不匹配的地图ID:~w", [_RoleId, _MapId]),
    {noreply, Map};


%% 角色离开地图
handle_info({role_leave, RoleId}, Map) ->
    do_leave(Map, RoleId),
    {noreply, Map};

%% 更新ConnPid
handle_info({role_update_conn_pid, RoleId, ConnPid}, Map) ->
    case get({role, RoleId}) of
        Mr = #map_role{gx = Gx, gy = Gy} ->
            put({role, RoleId}, Mr#map_role{conn_pid = ConnPid}),
            put(role_conn, [ConnPid | lists:delete(ConnPid, get(role_conn))]),
            put({Gx, Gy}, [{RoleId, ConnPid} | lists:keydelete(RoleId, 1, get({Gx, Gy}))]);
        _ -> ignore
    end,
    {noreply, Map};

%% 广播数据
handle_info({send_to_all, Bin}, Map) ->
    do_cast(get(role_conn), Bin),
    {noreply, Map};

%% 广播移动信息
handle_info(cast_move, Map) ->
    case get(role_move) of
        [] -> ignore;
        L = [_|_] ->
            L1 = lists:reverse(L),
            cast_move(L1),
            put(role_move, [])
    end,
    erlang:send_after(?MAP_CAST_MOVE_INTERVAL, self(), cast_move),
    {noreply, Map};

%% 关闭地图
handle_info(stop, Map) ->
    {stop, normal, Map};

handle_info(_Info, Map) ->
    ?ERR("收到未知消息: ~w", [_Info]),
    {noreply, Map}.

terminate(_Reason, #map{id = Id, base_id = _BaseId}) ->
    ets:delete(map_info, Id),
    ?DEBUG("关闭地图：Id:~w BaseId:~w", [Id, _BaseId]),
    ok.

code_change(_OldVsn, Map, _Extra) ->
    {ok, Map}.

%% ----------------------------------------------------
%% 私有函数
%% ----------------------------------------------------
%% 处理进入地图的操作 -> ok
do_enter(#map{id = MapId, base_id = MapBaseId, width = W, height = H}, MapRole = #map_role{id = RoleId, conn_pid = ConnPid, x = X, y = Y}) ->
    X1 = ?DX(X, W),
    Y1 = ?DY(Y, H),
    Gx = ?GX(X1),
    Gy = ?GY(Y1),
    Mr = MapRole#map_role{x = X1, y = Y1, gx = Gx, gy = Gy},
    put({role, RoleId}, Mr),
    put(role_conn, [ConnPid | get(role_conn)]), %% 添加到角色索引中
    put({Gx, Gy}, [{RoleId, ConnPid} | get({Gx, Gy})]),
    %% 通知客户端切换地图
    sys_conn:pack_send(ConnPid, 10120, {MapId, MapBaseId, X1, Y1}),
    put(role_move, [#map_role_move{type = ?MAP_ROLE_MOVE_TYPE_ENTER_MAP, args = {RoleId, Gx, Gy}} | get(role_move)]),
    ok.

%% 处理离开地图操作 -> ok
do_leave(#map{id = MapId, base_id = MapBaseId}, RoleId) ->
    case erase({role, RoleId}) of
        #map_role{id = RoleId, conn_pid = ConnPid, name = _Name, gx = Gx, gy = Gy} ->
            put(role_conn, lists:delete(ConnPid, get(role_conn))),
            put({Gx, Gy}, lists:keydelete(RoleId, 1, get({Gx, Gy}))),
            put(role_move, [#map_role_move{type = ?MAP_ROLE_MOVE_TYPE_LEAVE_MAP, args = {RoleId, MapId, MapBaseId, Gx, Gy}} | get(role_move)]),
            erase({last_move, RoleId});
        _ ->
            ?DEBUG("[~w]不在此地图[~w, ~w]，离开失败", [RoleId, MapId, MapBaseId])
    end,
    ok.

%% 处理地图移动同步位置
%% （坐标轴原点在左下角）
%% do_move_update_pos(#map_role{}, {SrcX, SrcY}, {DestX1, DestY1}, {SrcGx, SrcGy}, {DestGx1, DestGy1}) -> ok.
do_move_update_pos(_, _, _, {SrcGx, SrcGy}, {DestGx, DestGy})
when SrcGx =:= DestGx andalso SrcGy =:= DestGy -> %% 同一个大格子内移动
    ok;
do_move_update_pos(Mr, {SrcX, SrcY}, {DestX, DestY}, {SrcGx, SrcGy}, {DestGx, DestGy})
when SrcGx =:= DestGx andalso SrcGy < DestGy -> %% 向上移动
    cross_grid(
        Mr,
        {SrcX, SrcY}, {DestX, DestY},
        {SrcGx, SrcGy}, {DestGx, DestGy},
        [{SrcGx - 1, SrcGy - 1}, {SrcGx, SrcGy - 1}, {SrcGx + 1, SrcGy - 1}],
        [{DestGx - 1, DestGy + 1}, {DestGx, DestGy + 1}, {DestGx + 1, DestGy + 1}]
    );
do_move_update_pos(Mr, {SrcX, SrcY}, {DestX, DestY}, {SrcGx, SrcGy}, {DestGx, DestGy})
when SrcGx =:= DestGx andalso SrcGy > DestGy -> %% 向下移动
    cross_grid(
        Mr,
        {SrcX, SrcY}, {DestX, DestY},
        {SrcGx, SrcGy}, {DestGx, DestGy},
        [{SrcGx - 1, SrcGy + 1}, {SrcGx, SrcGy + 1}, {SrcGx + 1, SrcGy + 1}],
        [{DestGx - 1, DestGy - 1}, {DestGx, DestGy - 1}, {DestGx + 1, DestGy - 1}]
    );
do_move_update_pos(Mr, {SrcX, SrcY}, {DestX, DestY}, {SrcGx, SrcGy}, {DestGx, DestGy})
when SrcGx > DestGx andalso SrcGy =:= DestGy -> %% 向左移动
    cross_grid(
        Mr,
        {SrcX, SrcY}, {DestX, DestY},
        {SrcGx, SrcGy}, {DestGx, DestGy},
        [{SrcGx + 1, SrcGy - 1}, {SrcGx + 1, SrcGy}, {SrcGx + 1, SrcGy + 1}],
        [{DestGx - 1, DestGy - 1}, {DestGx - 1, DestGy}, {DestGx - 1, DestGy + 1}]
    );
do_move_update_pos(Mr, {SrcX, SrcY}, {DestX, DestY}, {SrcGx, SrcGy}, {DestGx, DestGy})
when SrcGx < DestGx andalso SrcGy =:= DestGy -> %% 向右移动
    cross_grid(
        Mr,
        {SrcX, SrcY}, {DestX, DestY},
        {SrcGx, SrcGy}, {DestGx, DestGy},
        [{SrcGx - 1, SrcGy - 1}, {SrcGx - 1, SrcGy}, {SrcGx - 1, SrcGy + 1}],
        [{DestGx + 1, DestGy - 1}, {DestGx + 1, DestGy}, {DestGx + 1, DestGy + 1}]
    );
do_move_update_pos(Mr, {SrcX, SrcY}, {DestX, DestY}, {SrcGx, SrcGy}, {DestGx, DestGy})
when SrcGx > DestGx andalso SrcGy > DestGy -> %% 向左上移动
    cross_grid(
        Mr,
        {SrcX, SrcY}, {DestX, DestY},
        {SrcGx, SrcGy}, {DestGx, DestGy},
        [{SrcGx + 1, SrcGy - 1}, {SrcGx + 1, SrcGy}, {SrcGx + 1, SrcGy + 1}, {SrcGx, SrcGy + 1}, {SrcGx - 1, SrcGy + 1}],
        [{DestGx - 1, DestGy + 1}, {DestGx - 1, DestGy}, {DestGx - 1, DestGy - 1}, {DestGx, DestGy - 1}, {DestGx + 1, DestGy - 1}]
    );
do_move_update_pos(Mr, {SrcX, SrcY}, {DestX, DestY}, {SrcGx, SrcGy}, {DestGx, DestGy})
when SrcGx > DestGx andalso SrcGy < DestGy -> %% 向左下移动
    cross_grid(
        Mr,
        {SrcX, SrcY}, {DestX, DestY},
        {SrcGx, SrcGy}, {DestGx, DestGy},
        [{SrcGx - 1, SrcGy - 1}, {SrcGx, SrcGy - 1}, {SrcGx + 1, SrcGy - 1}, {SrcGx + 1, SrcGy}, {SrcGx + 1, SrcGy + 1}],
        [{DestGx - 1, DestGy - 1}, {DestGx - 1, DestGy}, {DestGx - 1, DestGy + 1}, {DestGx, DestGy + 1}, {DestGx + 1, DestGy + 1}]
    );
do_move_update_pos(Mr, {SrcX, SrcY}, {DestX, DestY}, {SrcGx, SrcGy}, {DestGx, DestGy})
when SrcGx < DestGx andalso SrcGy > DestGy -> %% 向右上移动
    cross_grid(
        Mr,
        {SrcX, SrcY}, {DestX, DestY},
        {SrcGx, SrcGy}, {DestGx, DestGy},
        [{SrcGx - 1, SrcGy - 1}, {SrcGx, SrcGy}, {SrcGx - 1, SrcGy + 1}, {SrcGx, SrcGy + 1}, {SrcGx + 1, SrcGy + 1}],
        [{DestGx - 1, DestGy - 1}, {DestGx, DestGy - 1}, {DestGx + 1, DestGy - 1}, {DestGx + 1, DestGy}, {DestGx + 1, DestGy + 1}]
    );
do_move_update_pos(Mr, {SrcX, SrcY}, {DestX, DestY}, {SrcGx, SrcGy}, {DestGx, DestGy})
when SrcGx < DestGx andalso SrcGy < DestGy -> %% 向右下移动
    cross_grid(
        Mr,
        {SrcX, SrcY}, {DestX, DestY},
        {SrcGx, SrcGy}, {DestGx, DestGy},
        [{SrcGx - 1, SrcGy + 1}, {SrcGx - 1, SrcGy}, {SrcGx - 1, SrcGy - 1}, {SrcGx, SrcGy - 1}, {SrcGx + 1, SrcGy - 1}],
        [{DestGx + 1, DestGy - 1}, {DestGx + 1, DestGy}, {DestGx + 1, DestGy + 1}, {DestGx, DestGy + 1}, {DestGx - 1, DestGy + 1}]
    ).

%% 初始化广播格子
init_grids(#map{width = W, height = H}) ->
    GW = ?GW(W),
    GH = ?GH(H),
    util:for(1, GH, fun(Y) ->
                util:for(1, GW, fun(X) ->
                            put({X, Y}, [])
                    end)
        end).

%% 获取可视范围内的所有#map_role{}列表 -> [#map_role{} | pid()]
get_in_range(Label, Gx, Gy) ->
    do_get_in_range(
        Label,
        [
            {Gx-1, Gy-1}, {Gx, Gy-1}, {Gx+1, Gy-1},
            {Gx-1, Gy}, {Gx, Gy}, {Gx+1, Gy},
            {Gx-1, Gy+1}, {Gx, Gy+1}, {Gx+1, Gy+1}
        ],
        []).
do_get_in_range(_, [], Result) -> Result;
do_get_in_range(Label, [{Gx, Gy} | T], Result) ->
    Result1 = do_do_get_in_range(Label, get({Gx, Gy}), Result),
    do_get_in_range(Label, T, Result1).

do_do_get_in_range(_, [], Result) -> Result;
do_do_get_in_range(_, undefined, Result) -> Result;
do_do_get_in_range(map_role, [{RoleId, _} | T], Result) ->
    case get({role, RoleId}) of
        undefined -> do_do_get_in_range(map_role, T, Result);
        Mr -> do_do_get_in_range(map_role, T, [Mr | Result])
    end;
do_do_get_in_range(conn_pid, [{_, ConnPid} | T], Result) ->
    do_do_get_in_range(conn_pid, T, [ConnPid | Result]).

%% 格子广播
grid_cast(Gx, Gy, Bin) ->
    ConnPids = get_in_range(conn_pid, Gx, Gy),
    do_cast(ConnPids, Bin).
grid_cast([], _Bin) -> ok;
grid_cast([{Gx, Gy} | T], Bin) ->
    ConnPids = get_in_range(conn_pid, Gx, Gy),
    do_cast(ConnPids, Bin),
    grid_cast(T, Bin).
do_cast([], _) -> ok;
do_cast([ConnPid | T], Bin) ->
    sys_conn:send(ConnPid, Bin),
    do_cast(T, Bin).

%% 跨格移动
%%（1）通知离开的格子的人
%%（2）通知新进格子的人，并告诉他们移动目的地
cross_grid(#map_role{id = RoleId, conn_pid = ConnPid}, {_SrcX, _SrcY}, {_DestX, _DestY}, {SrcGx, SrcGy}, {DestGx, DestGy}, Leave, Enter) ->
    %% 从原有的格子中移除，并加入到新格子中
    put({SrcGx, SrcGy}, lists:keydelete(RoleId, 1, get({SrcGx, SrcGy}))),
    put({DestGx, DestGy}, [{RoleId, ConnPid} | get({DestGx, DestGy})]),
    put(role_move, [#map_role_move{type = ?MAP_ROLE_MOVE_TYPE_LEAVE_GRID, args = {RoleId, Leave}} | get(role_move)]),
    put(role_move, [#map_role_move{type = ?MAP_ROLE_MOVE_TYPE_ENTER_GRID, args = {RoleId, Enter}} | get(role_move)]).

%% 发送角色列表
send_role_list(_ConnPid, []) -> ok;
send_role_list(ConnPid, Mrs) ->
    {ok, BinList} = proto_101:pack(srv, 10121, {Mrs}),
    ConnPid ! {send_data, BinList}.

%% 分类处理堆积的移动信息
cast_move([]) -> ok;
cast_move(Moves) -> cast_move(Moves, []).

cast_move([], L) -> do_cast_move(L);
cast_move([#map_role_move{type = ?MAP_ROLE_MOVE_TYPE_MOVE, args = Args} | T], L) ->
    cast_move(T, [Args | L]);
cast_move([#map_role_move{type = ?MAP_ROLE_MOVE_TYPE_ENTER_GRID, args = Args} | T], L) ->
    do_cast_move(L),
    do_cast_enter_grid(Args),
    cast_move(T, []);
cast_move([#map_role_move{type = ?MAP_ROLE_MOVE_TYPE_LEAVE_GRID, args = Args} | T], L) ->
    do_cast_move(L),
    do_cast_leave_grid(Args),
    cast_move(T, []);
cast_move([#map_role_move{type = ?MAP_ROLE_MOVE_TYPE_ENTER_MAP, args = Args} | T], L) ->
    do_cast_move(L),
    do_cast_enter_map(Args),
    cast_move(T, []);
cast_move([#map_role_move{type = ?MAP_ROLE_MOVE_TYPE_LEAVE_MAP, args = Args} | T], L) ->
    do_cast_move(L),
    do_cast_leave_map(Args),
    cast_move(T, []).

do_cast_move([]) -> ok;
do_cast_move(L = [_|_]) ->
    L1 = group_by_grid(L),
    lists:foreach(fun({{Gx, Gy}, Moves}) ->
                case Moves of
                    [] -> ignore;
                    [_|_] ->
                        {ok, BinMove} = proto_101:pack(srv, 10151, {Moves}),
                        grid_cast(Gx, Gy, BinMove)
                end
        end, L1).

do_cast_leave_grid({RoleId, Leave}) ->
    {ok, BinLeave} = proto_101:pack(srv, 10150, {[], [RoleId]}),
    grid_cast(Leave, BinLeave).

do_cast_enter_grid({RoleId, Enter}) ->
    case get({role, RoleId}) of
        Mr = #map_role{conn_pid = ConnPid} ->
            %% 广播此角色进入消息给新进格子的人
            Mr1 = append_last_move(Mr),
            {ok, BinEnter} = proto_101:pack(srv, 10150, {[Mr1], []}),
            grid_cast(Enter, BinEnter),
            %% 通知此角色新进的格子有哪些人
            EnterMrs = do_get_in_range(map_role, Enter, []),
            EnterMrs1 = [append_last_move(Mr0) || Mr0 <- EnterMrs],
            send_role_list(ConnPid, EnterMrs1);
        _ ->
            ignore
    end.

do_cast_enter_map({RoleId, Gx, Gy}) ->
    case get({role, RoleId}) of
        Mr = #map_role{conn_pid = ConnPid} ->
            %% 推送可视范围内的角色列表
            EnterMrs = [append_last_move(Mr0) || Mr0 <- get_in_range(map_role, Gx, Gy)],
            sys_conn:pack_send(ConnPid, 10121, {EnterMrs}),
            %% 广播进入消息给附近的角色
            {ok, BinEnter} = proto_101:pack(srv, 10150, {[Mr], []}),
            grid_cast(Gx, Gy, BinEnter);
        _ ->
            ignore
    end.

do_cast_leave_map({{Rid, P, Z}, MapId, MapBaseId, Gx, Gy}) ->
    {ok, BinLeave} = proto_101:pack(srv, 10152, {[{Rid, P, Z, MapId, MapBaseId}]}),
    grid_cast(Gx, Gy, BinLeave).

%% 获取最近一次移动 -> {SrcX, SrcY, DestX, DestY, Dir}
get_last_move(RoleId) ->
    case get({last_move, RoleId}) of
        #map_role_move{type = ?MAP_ROLE_MOVE_TYPE_MOVE, args = {RoleId, SrcX, SrcY, DestX, DestY, Dir, _SrcGx, _SrcGy}} ->
            {SrcX, SrcY, DestX, DestY, Dir};
        _ ->
            {0, 0, 0, 0, 0}
    end.

%% 附加最近一次移动信息到#map_role{}
append_last_move(Mr = #map_role{id = RoleId}) ->
    {SrcX, SrcY, DestX, DestY, Dir} = get_last_move(RoleId),
    Mr#map_role{last_move_src_x = SrcX, last_move_src_y = SrcY, last_move_dest_x = DestX, last_move_dest_y = DestY, last_move_dir = Dir}.

%% 按照广播格子分组 -> {{Gx, Gy}, [{Rid, Platform, ZoneId, SrcX, SrcY, DestX, DestY, Dir}]}
group_by_grid(Moves) -> group_by_grid(Moves, dict:new()).
group_by_grid([], Dict) -> dict:to_list(Dict);
group_by_grid([{RoleId, SrcX, SrcY, DestX, DestY, Dir, SrcGx, SrcGy} | T], Dict) ->
    {Rid, P, Z} = RoleId,
    Key = {SrcGx, SrcGy},
    Value = {Rid, P, Z, SrcX, SrcY, DestX, DestY, Dir},
    case dict:find(Key, Dict) of
        {ok, _} ->
            Dict1 = dict:append(Key, Value, Dict),
            group_by_grid(T, Dict1);
        error ->
            Dict1 = dict:store(Key, [Value], Dict),
            group_by_grid(T, Dict1)
    end.
