%%----------------------------------------------------
%% 地图相关RPC调用
%%----------------------------------------------------
-module(map_rpc).
-export([handle/3]).
-include("common.hrl").
-include("role.hrl").
-include("map.hrl").
-include("pos.hrl").
-include("link.hrl").

%% 角色进入地图
handle(10100, _, Role = #role{pos = Pos}) ->
    {ToMapId, ToX, ToY} = case Pos of
        undefined ->
            #map_data{revive = Entry} = map_data:get(?DEFAULT_MAP_ID),
            {X, Y} = util:rand_list(Entry),
            {?DEFAULT_MAP_ID, X, Y};
        #pos{map_id = MapId, map_base_id = MapBaseId, x = X, y = Y, last = _Last} ->
            #map_data{revive = Entry} = map_data:get(MapBaseId),
            case map:is_blocked(MapId, MapBaseId, X, Y) of
                false -> {MapId, X, Y};
                true ->
                    {X, Y} = util:rand_list(Entry),
                    {MapId, X, Y}
            end
    end,
    case map:role_enter({ToMapId, ToX, ToY}, Role) of
        {ok, NewRole} -> {ok, NewRole};
        {error, Msg} ->
            %% TODO:如果进不去正常的地图，可以考虑进入上次进入的地方
            {reply, {?false, Msg}}
    end;

%% 客户端通知地图加载完成
handle(10101, _, #role{name = _Name}) ->
    ?DEBUG("角色[~ts]通知加载场景完成", [_Name]),
    ok;

%% 角色移动
handle(10102, _, #role{pos = #pos{map_pid = MapPid}}) when not is_pid(MapPid) ->
    ok;
handle(10102, _, _Role = #role{status = Status}) when Status =/= ?status_normal -> 
    ?DEBUG("角色[~ts]非正常状态请求移动", [_Role#role.name]),
    ok;
handle(10102, {MapId, X, Y, Dir}, Role) ->
    %% ?DEBUG("角色[~ts]发来移动包:~w", [Role#role.name, {MapId, X, Y, Dir}]),
    map:role_move(MapId, X, Y, Dir, Role),
    ok;

%% 角色移动过程中同步位置
handle(10103, _, #role{pos = #pos{map_pid = MapPid}}) when not is_pid(MapPid) ->
    ok;
handle(10103, _, _Role = #role{status = Status}) when Status =/= ?status_normal -> 
    ?DEBUG("角色[~ts]非正常状态请求移动同步", [_Role#role.name]),
    ok;
handle(10103, {MapId, X, Y, Dir}, Role) ->
    %% ?DEBUG("角色[~ts]发来移动同步包:~w", [Role#role.name, {MapId, X, Y, Dir}]),
    map:role_move_update_pos(MapId, X, Y, Dir, Role),
    ok;


%% 容错匹配
handle(_Cmd, _Data, _Role) ->
    ?DEBUG("模块[~w]收到无效的RPC调用[~w]: ~w", [?MODULE, _Cmd, _Data]),
    {error, unknow_command}.

%% ----------------------------------------------------
%% 私有函数
%% ----------------------------------------------------
