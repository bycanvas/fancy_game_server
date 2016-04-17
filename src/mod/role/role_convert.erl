%%----------------------------------------------------
%% 角色数据转换
%%----------------------------------------------------
-module(role_convert).
-export([to/2]).
-include("common.hrl").
-include("role.hrl").
-include("link.hrl").
-include("pos.hrl").
-include("map.hrl").

%% @doc 将角色数据转换成其它格式的数据
-spec to(Type, Role) -> {ok, tuple()} | {error, role_convert_unknow_type} when
    Type :: atom(),
    Role :: #role{}.

to(role, R = #role{}) -> {ok, R};

%% =========================================
%% 模块数据转换
%% =========================================
to(map_role, #role{id = RoleId = {Rid, P, Zid}, pid = Pid, link = #link{conn_pid = ConnPid}, name = Name, lev = Lev, hp = Hp, hp_max = HpMax, status = Status, action = Action, speed = Speed, pos = #pos{map_id = MapId, x = X, y = Y, dir = Dir}}) ->
    {ok, #map_role{
            id = RoleId, pid = Pid, conn_pid = ConnPid, rid = Rid, platform = P, zone_id = Zid
            ,name = Name
            ,speed = Speed, map = MapId, x = X, y = Y, dir = Dir
            ,status = Status, action = Action
            ,lev = Lev, hp_max = HpMax, hp = Hp
        }
    };

to(online_cache, #role{id = RoleId, name = Name, pid = Pid}) ->
    {ok, #role_online_cache{
            id = RoleId, name = Name, pid = Pid
        }
    };

%% 容错
to(_Type, _R) ->
    {error, role_convert_unknow_type}.
