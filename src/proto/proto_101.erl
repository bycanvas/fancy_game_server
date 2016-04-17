%%----------------------------------------------------
%% 地图信息推送、广播 
%% 该文件由程序生成，不要手动修改
%%----------------------------------------------------
-module(proto_101).
-export([
        pack/3
        ,unpack/3
    ]
).
-include("common.hrl").
-include("map.hrl").
-include("role.hrl").


-define(TO_BIN(S), case is_list(S) of
        true -> unicode:characters_to_binary(S);
        _ -> S
    end).
%%----------------------------------------------------
%% @doc 打包命令
%%----------------------------------------------------
-spec pack(srv | cli, non_neg_integer(), tuple()) ->
    {ok, binary()} | {error, {unknown_pack_command, non_neg_integer()}}.

pack(cli, 10100, {}) ->
    {ok, <<2:32, 10100:16>>};

pack(srv, 10100, {V0_result, V0_msg}) ->
    D_a_t_a = <<V0_result:8, (byte_size(?TO_BIN(V0_msg))):16, (?TO_BIN(V0_msg))/binary>>,
    {ok, <<(byte_size(D_a_t_a) + 2):32, 10100:16, D_a_t_a/binary>>};

pack(cli, 10101, {}) ->
    {ok, <<2:32, 10101:16>>};

pack(srv, 10101, {}) ->
    {ok, <<2:32, 10101:16>>};

pack(cli, 10102, {V0_map_id, V0_x, V0_y, V0_dir}) ->
    D_a_t_a = <<V0_map_id:32, V0_x:16, V0_y:16, V0_dir:8>>,
    {ok, <<(byte_size(D_a_t_a) + 2):32, 10102:16, D_a_t_a/binary>>};

pack(srv, 10102, {V0_result, V0_msg}) ->
    D_a_t_a = <<V0_result:8, (byte_size(?TO_BIN(V0_msg))):16, (?TO_BIN(V0_msg))/binary>>,
    {ok, <<(byte_size(D_a_t_a) + 2):32, 10102:16, D_a_t_a/binary>>};

pack(cli, 10103, {V0_map_id, V0_x, V0_y, V0_dir}) ->
    D_a_t_a = <<V0_map_id:32, V0_x:16, V0_y:16, V0_dir:8>>,
    {ok, <<(byte_size(D_a_t_a) + 2):32, 10103:16, D_a_t_a/binary>>};

pack(srv, 10103, {V0_result, V0_msg}) ->
    D_a_t_a = <<V0_result:8, (byte_size(?TO_BIN(V0_msg))):16, (?TO_BIN(V0_msg))/binary>>,
    {ok, <<(byte_size(D_a_t_a) + 2):32, 10103:16, D_a_t_a/binary>>};

pack(cli, 10120, {}) ->
    {ok, <<2:32, 10120:16>>};

pack(srv, 10120, {V0_map_id, V0_base_id, V0_x, V0_y}) ->
    D_a_t_a = <<V0_map_id:32, V0_base_id:32, V0_x:16, V0_y:16>>,
    {ok, <<(byte_size(D_a_t_a) + 2):32, 10120:16, D_a_t_a/binary>>};

pack(cli, 10121, {}) ->
    {ok, <<2:32, 10121:16>>};

pack(srv, 10121, {V0_role_list}) ->
    D_a_t_a = <<(length(V0_role_list)):16, (list_to_binary([<<V1_rid:32, (byte_size(?TO_BIN(V1_platform))):16, (?TO_BIN(V1_platform))/binary, V1_zone_id:16, (byte_size(?TO_BIN(V1_name))):16, (?TO_BIN(V1_name))/binary, V1_lev:8, V1_status:8, V1_action:8, V1_speed:16, V1_hp_max:32, V1_hp:32, V1_x:16, V1_y:16, V1_gx:16, V1_gy:16>> || #map_role{rid = V1_rid, platform = V1_platform, zone_id = V1_zone_id, name = V1_name, lev = V1_lev, status = V1_status, action = V1_action, speed = V1_speed, hp_max = V1_hp_max, hp = V1_hp, x = V1_x, y = V1_y, gx = V1_gx, gy = V1_gy} <- V0_role_list]))/binary>>,
    {ok, <<(byte_size(D_a_t_a) + 2):32, 10121:16, D_a_t_a/binary>>};

pack(cli, 10150, {}) ->
    {ok, <<2:32, 10150:16>>};

pack(srv, 10150, {V0_enter_role_list, V0_leave_role_list}) ->
    D_a_t_a = <<(length(V0_enter_role_list)):16, (list_to_binary([<<V1_rid:32, (byte_size(?TO_BIN(V1_platform))):16, (?TO_BIN(V1_platform))/binary, V1_zone_id:16, (byte_size(?TO_BIN(V1_name))):16, (?TO_BIN(V1_name))/binary, V1_lev:8, V1_status:8, V1_action:8, V1_speed:16, V1_hp_max:32, V1_hp:32, V1_x:16, V1_y:16, V1_gx:16, V1_gy:16, V1_last_move_src_x:16, V1_last_move_src_y:16, V1_last_move_dest_x:16, V1_last_move_dest_y:16, V1_last_move_dir:8>> || #map_role{rid = V1_rid, platform = V1_platform, zone_id = V1_zone_id, name = V1_name, lev = V1_lev, status = V1_status, action = V1_action, speed = V1_speed, hp_max = V1_hp_max, hp = V1_hp, x = V1_x, y = V1_y, gx = V1_gx, gy = V1_gy, last_move_src_x = V1_last_move_src_x, last_move_src_y = V1_last_move_src_y, last_move_dest_x = V1_last_move_dest_x, last_move_dest_y = V1_last_move_dest_y, last_move_dir = V1_last_move_dir} <- V0_enter_role_list]))/binary, (length(V0_leave_role_list)):16, (list_to_binary([<<V1_rid:32, (byte_size(?TO_BIN(V1_platform))):16, (?TO_BIN(V1_platform))/binary, V1_zone_id:16>> || {V1_rid, V1_platform, V1_zone_id} <- V0_leave_role_list]))/binary>>,
    {ok, <<(byte_size(D_a_t_a) + 2):32, 10150:16, D_a_t_a/binary>>};

pack(cli, 10151, {}) ->
    {ok, <<2:32, 10151:16>>};

pack(srv, 10151, {V0_role_list}) ->
    D_a_t_a = <<(length(V0_role_list)):16, (list_to_binary([<<V1_rid:32, (byte_size(?TO_BIN(V1_platform))):16, (?TO_BIN(V1_platform))/binary, V1_zone_id:16, V1_x:16, V1_y:16, V1_dest_x:16, V1_dest_y:16, V1_dir:8>> || {V1_rid, V1_platform, V1_zone_id, V1_x, V1_y, V1_dest_x, V1_dest_y, V1_dir} <- V0_role_list]))/binary>>,
    {ok, <<(byte_size(D_a_t_a) + 2):32, 10151:16, D_a_t_a/binary>>};

pack(cli, 10152, {}) ->
    {ok, <<2:32, 10152:16>>};

pack(srv, 10152, {V0_role_list}) ->
    D_a_t_a = <<(length(V0_role_list)):16, (list_to_binary([<<V1_rid:32, (byte_size(?TO_BIN(V1_platform))):16, (?TO_BIN(V1_platform))/binary, V1_zone_id:16, V1_map_id:32, V1_base_id:32>> || {V1_rid, V1_platform, V1_zone_id, V1_map_id, V1_base_id} <- V0_role_list]))/binary>>,
    {ok, <<(byte_size(D_a_t_a) + 2):32, 10152:16, D_a_t_a/binary>>};

pack(cli, 10153, {}) ->
    {ok, <<2:32, 10153:16>>};

pack(srv, 10153, {V0_role_list}) ->
    D_a_t_a = <<(length(V0_role_list)):16, (list_to_binary([<<V1_rid:32, (byte_size(?TO_BIN(V1_platform))):16, (?TO_BIN(V1_platform))/binary, V1_zone_id:16, (byte_size(?TO_BIN(V1_name))):16, (?TO_BIN(V1_name))/binary, V1_lev:8, V1_status:8, V1_action:8, V1_speed:16, V1_hp_max:32, V1_hp:32, V1_x:16, V1_y:16, V1_gx:16, V1_gy:16>> || #map_role{rid = V1_rid, platform = V1_platform, zone_id = V1_zone_id, name = V1_name, lev = V1_lev, status = V1_status, action = V1_action, speed = V1_speed, hp_max = V1_hp_max, hp = V1_hp, x = V1_x, y = V1_y, gx = V1_gx, gy = V1_gy} <- V0_role_list]))/binary>>,
    {ok, <<(byte_size(D_a_t_a) + 2):32, 10153:16, D_a_t_a/binary>>};

pack(_Type, Code, _Data) ->
    {error, {unknown_pack_command, Code}}.

%%----------------------------------------------------
%% @doc 解包命令
%%----------------------------------------------------
-spec unpack(srv | cli, non_neg_integer(), binary()) ->
    {ok, tuple()} | {error, {unknown_unpack_command, non_neg_integer()}}.

unpack(srv, 10100, _B0) ->
    {ok, {}};

unpack(cli, 10100, _B0) ->
    {V1_result, _B1} = protocol:uint8(_B0),
    {V1_msg, _B2} = protocol:string(_B1),
    {ok, {V1_result, V1_msg}};

unpack(srv, 10101, _B0) ->
    {ok, {}};

unpack(cli, 10101, _B0) ->
    {ok, {}};

unpack(srv, 10102, _B0) ->
    {V1_map_id, _B1} = protocol:uint32(_B0),
    {V1_x, _B2} = protocol:uint16(_B1),
    {V1_y, _B3} = protocol:uint16(_B2),
    {V1_dir, _B4} = protocol:uint8(_B3),
    {ok, {V1_map_id, V1_x, V1_y, V1_dir}};

unpack(cli, 10102, _B0) ->
    {V1_result, _B1} = protocol:uint8(_B0),
    {V1_msg, _B2} = protocol:string(_B1),
    {ok, {V1_result, V1_msg}};

unpack(srv, 10103, _B0) ->
    {V1_map_id, _B1} = protocol:uint32(_B0),
    {V1_x, _B2} = protocol:uint16(_B1),
    {V1_y, _B3} = protocol:uint16(_B2),
    {V1_dir, _B4} = protocol:uint8(_B3),
    {ok, {V1_map_id, V1_x, V1_y, V1_dir}};

unpack(cli, 10103, _B0) ->
    {V1_result, _B1} = protocol:uint8(_B0),
    {V1_msg, _B2} = protocol:string(_B1),
    {ok, {V1_result, V1_msg}};

unpack(srv, 10120, _B0) ->
    {ok, {}};

unpack(cli, 10120, _B0) ->
    {V1_map_id, _B1} = protocol:uint32(_B0),
    {V1_base_id, _B2} = protocol:uint32(_B1),
    {V1_x, _B3} = protocol:uint16(_B2),
    {V1_y, _B4} = protocol:uint16(_B3),
    {ok, {V1_map_id, V1_base_id, V1_x, V1_y}};

unpack(srv, 10121, _B0) ->
    {ok, {}};

unpack(cli, 10121, _B0) ->
    {V1_role_list, _B17} = protocol:array(_B0, fun(_B1) ->
        {V2_rid, _B2} = protocol:uint32(_B1),
        {V2_platform, _B3} = protocol:string(_B2),
        {V2_zone_id, _B4} = protocol:uint16(_B3),
        {V2_name, _B5} = protocol:string(_B4),
        {V2_lev, _B6} = protocol:uint8(_B5),
        {V2_status, _B7} = protocol:uint8(_B6),
        {V2_action, _B8} = protocol:uint8(_B7),
        {V2_speed, _B9} = protocol:uint16(_B8),
        {V2_hp_max, _B10} = protocol:uint32(_B9),
        {V2_hp, _B11} = protocol:uint32(_B10),
        {V2_x, _B12} = protocol:uint16(_B11),
        {V2_y, _B13} = protocol:uint16(_B12),
        {V2_gx, _B14} = protocol:uint16(_B13),
        {V2_gy, _B15} = protocol:uint16(_B14),
        {{V2_rid, V2_platform, V2_zone_id, V2_name, V2_lev, V2_status, V2_action, V2_speed, V2_hp_max, V2_hp, V2_x, V2_y, V2_gx, V2_gy}, _B15}
    end),
    {ok, {V1_role_list}};

unpack(srv, 10150, _B0) ->
    {ok, {}};

unpack(cli, 10150, _B0) ->
    {V1_enter_role_list, _B22} = protocol:array(_B0, fun(_B1) ->
        {V2_rid, _B2} = protocol:uint32(_B1),
        {V2_platform, _B3} = protocol:string(_B2),
        {V2_zone_id, _B4} = protocol:uint16(_B3),
        {V2_name, _B5} = protocol:string(_B4),
        {V2_lev, _B6} = protocol:uint8(_B5),
        {V2_status, _B7} = protocol:uint8(_B6),
        {V2_action, _B8} = protocol:uint8(_B7),
        {V2_speed, _B9} = protocol:uint16(_B8),
        {V2_hp_max, _B10} = protocol:uint32(_B9),
        {V2_hp, _B11} = protocol:uint32(_B10),
        {V2_x, _B12} = protocol:uint16(_B11),
        {V2_y, _B13} = protocol:uint16(_B12),
        {V2_gx, _B14} = protocol:uint16(_B13),
        {V2_gy, _B15} = protocol:uint16(_B14),
        {V2_last_move_src_x, _B16} = protocol:uint16(_B15),
        {V2_last_move_src_y, _B17} = protocol:uint16(_B16),
        {V2_last_move_dest_x, _B18} = protocol:uint16(_B17),
        {V2_last_move_dest_y, _B19} = protocol:uint16(_B18),
        {V2_last_move_dir, _B20} = protocol:uint8(_B19),
        {{V2_rid, V2_platform, V2_zone_id, V2_name, V2_lev, V2_status, V2_action, V2_speed, V2_hp_max, V2_hp, V2_x, V2_y, V2_gx, V2_gy, V2_last_move_src_x, V2_last_move_src_y, V2_last_move_dest_x, V2_last_move_dest_y, V2_last_move_dir}, _B20}
    end),
    {V1_leave_role_list, _B28} = protocol:array(_B22, fun(_B23) ->
        {V2_rid, _B24} = protocol:uint32(_B23),
        {V2_platform, _B25} = protocol:string(_B24),
        {V2_zone_id, _B26} = protocol:uint16(_B25),
        {{V2_rid, V2_platform, V2_zone_id}, _B26}
    end),
    {ok, {V1_enter_role_list, V1_leave_role_list}};

unpack(srv, 10151, _B0) ->
    {ok, {}};

unpack(cli, 10151, _B0) ->
    {V1_role_list, _B11} = protocol:array(_B0, fun(_B1) ->
        {V2_rid, _B2} = protocol:uint32(_B1),
        {V2_platform, _B3} = protocol:string(_B2),
        {V2_zone_id, _B4} = protocol:uint16(_B3),
        {V2_x, _B5} = protocol:uint16(_B4),
        {V2_y, _B6} = protocol:uint16(_B5),
        {V2_dest_x, _B7} = protocol:uint16(_B6),
        {V2_dest_y, _B8} = protocol:uint16(_B7),
        {V2_dir, _B9} = protocol:uint8(_B8),
        {{V2_rid, V2_platform, V2_zone_id, V2_x, V2_y, V2_dest_x, V2_dest_y, V2_dir}, _B9}
    end),
    {ok, {V1_role_list}};

unpack(srv, 10152, _B0) ->
    {ok, {}};

unpack(cli, 10152, _B0) ->
    {V1_role_list, _B8} = protocol:array(_B0, fun(_B1) ->
        {V2_rid, _B2} = protocol:uint32(_B1),
        {V2_platform, _B3} = protocol:string(_B2),
        {V2_zone_id, _B4} = protocol:uint16(_B3),
        {V2_map_id, _B5} = protocol:uint32(_B4),
        {V2_base_id, _B6} = protocol:uint32(_B5),
        {{V2_rid, V2_platform, V2_zone_id, V2_map_id, V2_base_id}, _B6}
    end),
    {ok, {V1_role_list}};

unpack(srv, 10153, _B0) ->
    {ok, {}};

unpack(cli, 10153, _B0) ->
    {V1_role_list, _B17} = protocol:array(_B0, fun(_B1) ->
        {V2_rid, _B2} = protocol:uint32(_B1),
        {V2_platform, _B3} = protocol:string(_B2),
        {V2_zone_id, _B4} = protocol:uint16(_B3),
        {V2_name, _B5} = protocol:string(_B4),
        {V2_lev, _B6} = protocol:uint8(_B5),
        {V2_status, _B7} = protocol:uint8(_B6),
        {V2_action, _B8} = protocol:uint8(_B7),
        {V2_speed, _B9} = protocol:uint16(_B8),
        {V2_hp_max, _B10} = protocol:uint32(_B9),
        {V2_hp, _B11} = protocol:uint32(_B10),
        {V2_x, _B12} = protocol:uint16(_B11),
        {V2_y, _B13} = protocol:uint16(_B12),
        {V2_gx, _B14} = protocol:uint16(_B13),
        {V2_gy, _B15} = protocol:uint16(_B14),
        {{V2_rid, V2_platform, V2_zone_id, V2_name, V2_lev, V2_status, V2_action, V2_speed, V2_hp_max, V2_hp, V2_x, V2_y, V2_gx, V2_gy}, _B15}
    end),
    {ok, {V1_role_list}};

unpack(_Type, Code, _Data) ->
    {error, {unknown_unpack_command, Code}}.
