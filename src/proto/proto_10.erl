%%----------------------------------------------------
%% 客户端连接区服验证模块 
%% 该文件由程序生成，不要手动修改
%%----------------------------------------------------
-module(proto_10).
-export([
        pack/3
        ,unpack/3
    ]
).


-define(TO_BIN(S), case is_list(S) of
        true -> unicode:characters_to_binary(S);
        _ -> S
    end).
%%----------------------------------------------------
%% @doc 打包命令
%%----------------------------------------------------
-spec pack(srv | cli, non_neg_integer(), tuple()) ->
    {ok, binary()} | {error, {unknown_pack_command, non_neg_integer()}}.

pack(cli, 1001, {V0_account, V0_platform, V0_zone_id, V0_session_id}) ->
    D_a_t_a = <<(byte_size(?TO_BIN(V0_account))):16, (?TO_BIN(V0_account))/binary, (byte_size(?TO_BIN(V0_platform))):16, (?TO_BIN(V0_platform))/binary, V0_zone_id:16, (byte_size(?TO_BIN(V0_session_id))):16, (?TO_BIN(V0_session_id))/binary>>,
    {ok, <<(byte_size(D_a_t_a) + 2):32, 1001:16, D_a_t_a/binary>>};

pack(srv, 1001, {V0_result, V0_msg}) ->
    D_a_t_a = <<V0_result:8, (byte_size(?TO_BIN(V0_msg))):16, (?TO_BIN(V0_msg))/binary>>,
    {ok, <<(byte_size(D_a_t_a) + 2):32, 1001:16, D_a_t_a/binary>>};

pack(_Type, Code, _Data) ->
    {error, {unknown_pack_command, Code}}.

%%----------------------------------------------------
%% @doc 解包命令
%%----------------------------------------------------
-spec unpack(srv | cli, non_neg_integer(), binary()) ->
    {ok, tuple()} | {error, {unknown_unpack_command, non_neg_integer()}}.

unpack(srv, 1001, _B0) ->
    {V1_account, _B1} = protocol:string(_B0),
    {V1_platform, _B2} = protocol:string(_B1),
    {V1_zone_id, _B3} = protocol:uint16(_B2),
    {V1_session_id, _B4} = protocol:string(_B3),
    {ok, {V1_account, V1_platform, V1_zone_id, V1_session_id}};

unpack(cli, 1001, _B0) ->
    {V1_result, _B1} = protocol:uint8(_B0),
    {V1_msg, _B2} = protocol:string(_B1),
    {ok, {V1_result, V1_msg}};

unpack(_Type, Code, _Data) ->
    {error, {unknown_unpack_command, Code}}.
