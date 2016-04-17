%%----------------------------------------------------
%% 客户端连接验证模块 
%% 该文件由程序生成，不要手动修改
%%----------------------------------------------------
-module(proto_11).
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

pack(cli, 1101, {}) ->
    {ok, <<2:32, 1101:16>>};

pack(srv, 1101, {V0_result, V0_msg, V0_role_list}) ->
    D_a_t_a = <<V0_result:8, (byte_size(?TO_BIN(V0_msg))):16, (?TO_BIN(V0_msg))/binary, (length(V0_role_list)):16, (list_to_binary([<<V1_id_rid:32, (byte_size(?TO_BIN(V1_id_platform))):16, (?TO_BIN(V1_id_platform))/binary, V1_id_zone_id:16, (byte_size(?TO_BIN(V1_name))):16, (?TO_BIN(V1_name))/binary>> || {{V1_id_rid, V1_id_platform, V1_id_zone_id}, V1_name} <- V0_role_list]))/binary>>,
    {ok, <<(byte_size(D_a_t_a) + 2):32, 1101:16, D_a_t_a/binary>>};

pack(cli, 1102, {V0_name}) ->
    D_a_t_a = <<(byte_size(?TO_BIN(V0_name))):16, (?TO_BIN(V0_name))/binary>>,
    {ok, <<(byte_size(D_a_t_a) + 2):32, 1102:16, D_a_t_a/binary>>};

pack(srv, 1102, {V0_result, V0_msg, {V0_id_rid, V0_id_platform, V0_id_zone_id}}) ->
    D_a_t_a = <<V0_result:8, (byte_size(?TO_BIN(V0_msg))):16, (?TO_BIN(V0_msg))/binary, V0_id_rid:32, (byte_size(?TO_BIN(V0_id_platform))):16, (?TO_BIN(V0_id_platform))/binary, V0_id_zone_id:16>>,
    {ok, <<(byte_size(D_a_t_a) + 2):32, 1102:16, D_a_t_a/binary>>};

pack(cli, 1103, {V0_rid, V0_platform, V0_zone_id}) ->
    D_a_t_a = <<V0_rid:32, (byte_size(?TO_BIN(V0_platform))):16, (?TO_BIN(V0_platform))/binary, V0_zone_id:16>>,
    {ok, <<(byte_size(D_a_t_a) + 2):32, 1103:16, D_a_t_a/binary>>};

pack(srv, 1103, {V0_result, V0_msg}) ->
    D_a_t_a = <<V0_result:8, (byte_size(?TO_BIN(V0_msg))):16, (?TO_BIN(V0_msg))/binary>>,
    {ok, <<(byte_size(D_a_t_a) + 2):32, 1103:16, D_a_t_a/binary>>};

pack(cli, 1199, {}) ->
    {ok, <<2:32, 1199:16>>};

pack(srv, 1199, {V0_ts}) ->
    D_a_t_a = <<V0_ts:32>>,
    {ok, <<(byte_size(D_a_t_a) + 2):32, 1199:16, D_a_t_a/binary>>};

pack(_Type, Code, _Data) ->
    {error, {unknown_pack_command, Code}}.

%%----------------------------------------------------
%% @doc 解包命令
%%----------------------------------------------------
-spec unpack(srv | cli, non_neg_integer(), binary()) ->
    {ok, tuple()} | {error, {unknown_unpack_command, non_neg_integer()}}.

unpack(srv, 1101, _B0) ->
    {ok, {}};

unpack(cli, 1101, _B0) ->
    {V1_result, _B1} = protocol:uint8(_B0),
    {V1_msg, _B2} = protocol:string(_B1),
    {V1_role_list, _B9} = protocol:array(_B2, fun(_B3) ->
        {V2_id_rid, _B4} = protocol:uint32(_B3),
        {V2_id_platform, _B5} = protocol:string(_B4),
        {V2_id_zone_id, _B6} = protocol:uint16(_B5),
        {V2_name, _B7} = protocol:string(_B6),
        {{V2_id_rid, V2_id_platform, V2_id_zone_id, V2_name}, _B7}
    end),
    {ok, {V1_result, V1_msg, V1_role_list}};

unpack(srv, 1102, _B0) ->
    {V1_name, _B1} = protocol:string(_B0),
    {ok, {V1_name}};

unpack(cli, 1102, _B0) ->
    {V1_result, _B1} = protocol:uint8(_B0),
    {V1_msg, _B2} = protocol:string(_B1),
    {V1_id_rid, _B3} = protocol:uint32(_B2),
    {V1_id_platform, _B4} = protocol:string(_B3),
    {V1_id_zone_id, _B5} = protocol:uint16(_B4),
    {ok, {V1_result, V1_msg, V1_id_rid, V1_id_platform, V1_id_zone_id}};

unpack(srv, 1103, _B0) ->
    {V1_rid, _B1} = protocol:uint32(_B0),
    {V1_platform, _B2} = protocol:string(_B1),
    {V1_zone_id, _B3} = protocol:uint16(_B2),
    {ok, {V1_rid, V1_platform, V1_zone_id}};

unpack(cli, 1103, _B0) ->
    {V1_result, _B1} = protocol:uint8(_B0),
    {V1_msg, _B2} = protocol:string(_B1),
    {ok, {V1_result, V1_msg}};

unpack(srv, 1199, _B0) ->
    {ok, {}};

unpack(cli, 1199, _B0) ->
    {V1_ts, _B1} = protocol:uint32(_B0),
    {ok, {V1_ts}};

unpack(_Type, Code, _Data) ->
    {error, {unknown_unpack_command, Code}}.
