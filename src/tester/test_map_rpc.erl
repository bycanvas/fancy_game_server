%%----------------------------------------------------
%% 地图测试
%%----------------------------------------------------
-module(test_map_rpc).
-export([
        handle/3
        ,move_rand/1
        ,move_arrive/1
    ]
).
-include("common.hrl").
-include("tester.hrl").
-include("map.hrl").


move_rand(T = #tester{id = {_Rid, _P, _Z}, name = _Name, map_id = MapId, x = X, y = Y, width = W, height = H, speed = Speed}) ->
    R = util:rand(1, 10000),
    Dir = if
        R >= 0 andalso R < 2500 -> %% 向上
            case Y >= H of
                true -> ?MAP_ROLE_DIR_DOWN;
                false -> ?MAP_ROLE_DIR_UP
            end;
        R >= 2500 andalso R < 5000 -> %% 向右
            case X >= W of
                true -> ?MAP_ROLE_DIR_LEFT;
                false -> ?MAP_ROLE_DIR_RIGHT
            end;
        R >= 5000 andalso R < 7500 -> %% 向下
            case Y =< 1 of
                true -> ?MAP_ROLE_DIR_UP;
                false -> ?MAP_ROLE_DIR_DOWN
            end;
        true -> %% 向左
            case X =< 1 of
                true -> ?MAP_ROLE_DIR_RIGHT;
                false -> ?MAP_ROLE_DIR_LEFT
            end
    end,
    {DestX, DestY} = case Dir of
        ?MAP_ROLE_DIR_UP ->
            {X, util:rand(Y+1, H)};
        ?MAP_ROLE_DIR_RIGHT ->
            {util:rand(X+1, W), Y};
        ?MAP_ROLE_DIR_DOWN ->
            {X, util:rand(1, Y-1)};
        ?MAP_ROLE_DIR_LEFT ->
            {util:rand(1, X-1), Y}
    end,
    tester:pack_send(10102, {MapId, DestX, DestY, Dir}),
    ?T_DBG("[~w,~ts,~w, ~ts]请求移动:Src=(~w, ~w), Dest=(~w, ~w)", [_Rid, _P, _Z, _Name, X, Y, DestX, DestY]),
    put(is_moving, true),
    put(move_dest, {DestX, DestY, Dir}),
    PerTime = util:check_min(round(?MOVE_GRID_SIZE / Speed * 1000), 20),
    erlang:send_after(PerTime, self(), {move_check, PerTime}),
    {ok, T#tester{dir = Dir}}.

move_arrive(#tester{map_id = MapId, x = X, y = Y, dir = Dir}) ->
    tester:pack_send(10103, {MapId, X, Y, Dir}),
    ok.

handle(enter, [], _T) ->
    tester:pack_send(10100, {}),
    ok;

%% =========================================================================
handle(10100, {?false, _Msg}, _T) ->
    ?T_DBG("进入地图失败:~w", [_Msg]),
    stop;

handle(10120, {MapId, MapBaseId, X, Y}, T) ->
    tester:pack_send(10101, {}),
    #map_data{width = W, height = H} = map_data:get(MapBaseId),
    self() ! map_init_done,
    {ok, T#tester{map_id = MapId, map_base_id = MapBaseId, x = X, y = Y, dir = 1, width = W, height = H}};

handle(10121, _, _T) ->
    ok;

handle(10150, _, _T) ->
    ok;
handle(10151, _, _T) ->
    ok;
handle(10152, _, _T) ->
    ok;
handle(10153, _, _T) ->
    ok;

%% 容错处理
handle(_Cmd, _Data, #tester{name = _Name}) ->
    %% ?T_DBG("[~ts]收到未知消息[~w]: ~w", [_Name, _Cmd, _Data]),
    ok.
