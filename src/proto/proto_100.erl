%%----------------------------------------------------
%% 角色信息获取/初始化 
%% 该文件由程序生成，不要手动修改
%%----------------------------------------------------
-module(proto_100).
-export([
        pack/3
        ,unpack/3
    ]
).
-include("common.hrl").
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

pack(cli, 10000, {}) ->
    {ok, <<2:32, 10000:16>>};

pack(srv, 10000, {V0_rid, V0_platform, V0_zone_id, V0_name, V0_lev, V0_hp_max, V0_hp}) ->
    D_a_t_a = <<V0_rid:32, (byte_size(?TO_BIN(V0_platform))):16, (?TO_BIN(V0_platform))/binary, V0_zone_id:16, (byte_size(?TO_BIN(V0_name))):16, (?TO_BIN(V0_name))/binary, V0_lev:8, V0_hp_max:32, V0_hp:32>>,
    {ok, <<(byte_size(D_a_t_a) + 2):32, 10000:16, D_a_t_a/binary>>};

pack(_Type, Code, _Data) ->
    {error, {unknown_pack_command, Code}}.

%%----------------------------------------------------
%% @doc 解包命令
%%----------------------------------------------------
-spec unpack(srv | cli, non_neg_integer(), binary()) ->
    {ok, tuple()} | {error, {unknown_unpack_command, non_neg_integer()}}.

unpack(srv, 10000, _B0) ->
    {ok, {}};

unpack(cli, 10000, _B0) ->
    {V1_rid, _B1} = protocol:uint32(_B0),
    {V1_platform, _B2} = protocol:string(_B1),
    {V1_zone_id, _B3} = protocol:uint16(_B2),
    {V1_name, _B4} = protocol:string(_B3),
    {V1_lev, _B5} = protocol:uint8(_B4),
    {V1_hp_max, _B6} = protocol:uint32(_B5),
    {V1_hp, _B7} = protocol:uint32(_B6),
    {ok, {V1_rid, V1_platform, V1_zone_id, V1_name, V1_lev, V1_hp_max, V1_hp}};

unpack(_Type, Code, _Data) ->
    {error, {unknown_unpack_command, Code}}.
