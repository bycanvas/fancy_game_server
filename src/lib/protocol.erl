%%----------------------------------------------------
%% 协议相关处理
%%----------------------------------------------------
-module(protocol).
-export([
        pack/3
        ,unpack/3
        ,protocol_test/1

        ,byte/1
        ,string/1
        ,int8/1
        ,uint8/1
        ,int16/1
        ,uint16/1
        ,int32/1
        ,uint32/1
        ,float/1
        ,array/2
        ,read/2
        ,read/3
        ,read_fields/2
    ]
).
-include("common.hrl").
-include("protocol.hrl").

%% ------------------------------------------------------------
%% ProtoBuf协议解析库函数
%% ------------------------------------------------------------

%% @doc 将一个protobuf的结构数据打包为二进制
-spec pack(Parser::atom(), Code::pos_integer(), Data::iolist()) -> {ok, Bin::binary()}.
pack(Parser, Code, Data) ->
    PbList = Parser:encode(Data),
    DataBin = iolist_to_binary(PbList),
    {ok, <<(byte_size(DataBin) + 2):32, Code:16, DataBin/binary>>}.

%% @doc 解析协议二进制转换为可操作的record结构数据
-spec unpack(Parser::atom(), RecName::atom(), Bin::binary()) -> {ok, Rec::tuple()} | {error, undefined}.
unpack(Parser, Code, Bin) when is_integer(Code) ->
    case rec_name(Code) of
        Rec when is_atom(Rec) ->
            unpack(Parser, Rec, Bin);
        _ -> {error, undefined}
    end;
unpack(Parser, RecName, Bin) when is_atom(RecName) ->
    Rec = Parser:decode(RecName, Bin),
    case is_record(Rec, RecName) of
        true -> {ok, Rec};
        false -> {error, undefined}
    end.

%% @doc CMD转成协议名 -> atom()
-spec rec_name(Cmd::pos_integer()) -> RecName::atom().
rec_name(Cmd) ->
    proto_cmd:int_to_enum(msg_cmd_protocol_id, Cmd).

%% @doc 开启/关闭 协议发送测试
protocol_test(true) -> env:set(protocol_test, true);
protocol_test(_) -> env:set(protocol_test, undefined).


%% ------------------------------------------------------------
%% 以下是二进制自定义协议解析库函数
%% ------------------------------------------------------------

%% @doc 读取一个二进制流
-spec byte(binary()) -> {bitstring(), binary()}.
byte(<<L:32, Bin/binary>>) -> split_binary(Bin, L).

%% @doc 读取一个字符串
-spec string(binary()) -> {bitstring(), binary()}.
string(<<L:16, Bin/binary>>) -> split_binary(Bin, L).

%% @doc 读取一个无符号8位整数
-spec int8(binary()) -> {integer(), binary()}.
int8(<<N:8/signed, Bin/binary>>) -> {N, Bin}.

%% @doc 读取一个无符号8位整数
-spec uint8(binary()) -> {non_neg_integer(), binary()}.
uint8(<<N:8, Bin/binary>>) -> {N, Bin}.

%% @doc 读取一个有符号16位整数
-spec int16(binary()) -> {integer(), binary()}.
int16(<<N:16/signed, Bin/binary>>) -> {N, Bin}.

%% @doc 读取一个无符号16位整数
-spec uint16(binary()) -> {non_neg_integer(), binary()}.
uint16(<<N:16, Bin/binary>>) -> {N, Bin}.

%% @doc 读取一个有符号32位整数
-spec int32(binary()) -> {integer(), binary()}.
int32(<<N:32/signed, Bin/binary>>) -> {N, Bin}.

%% @doc 读取一个无符号32位整数
-spec uint32(binary()) -> {non_neg_integer(), binary()}.
uint32(<<N:32, Bin/binary>>) -> {N, Bin}.

%% @doc 读取一个有符号64位整数
-spec float(binary()) -> {float(), binary()}.
float(<<N:64/float, Bin/binary>>) -> {N, Bin}.

%% @doc 读取一个数组
-spec array(binary(), function()) -> {[tuple()], binary()}.
array(<<N:16, Bin/binary>>, Fun) -> array(N, Bin, Fun, []).
array(0, Bin, _Fun, List) -> {List, Bin};
array(N, Bin, Fun, List) ->
    {R, B} = Fun(Bin),
    array(N-1, B, Fun, [R | List]). 

%% @doc 读取一个字符串
-spec read(Type, binary()) -> {integer() | float() | [tuple()], binary()} when
    Type :: byte | string | int8 | uint8 | int16 | uint16 | int32 | uint32 | float.
read(byte, <<L:32, Bin/binary>>) ->
    split_binary(Bin, L);
read(string, <<L:16, Bin/binary>>) ->
    split_binary(Bin, L);
read(int8, <<N:8/signed, Bin/binary>>) ->
    {N, Bin};
read(uint8, <<N:8, Bin/binary>>) ->
    {N, Bin};
read(int16, <<N:16/signed, Bin/binary>>) ->
    {N, Bin};
read(uint16, <<N:16, Bin/binary>>) ->
    {N, Bin};
read(int32, <<N:32/signed, Bin/binary>>) ->
    {N, Bin};
read(uint32, <<N:32, Bin/binary>>) ->
    {N, Bin};
read(float, <<N:64/float, Bin/binary>>) ->
    {N, Bin}.
%% @doc 读取数组
-spec read(array, binary(), function()) -> {term(), binary()}.
read(array, <<N:16, Bin/binary>>, Fun) ->
    array(N, Bin, Fun, []).

%% @doc 按格式定义，动态解析二进行数据
%% <div>格式field: {类型, 名称, 描述} | {array, 名称, 描述, 子格式定义}</div>
-spec read_fields([tuple()], binary()) -> {tuple(), binary()}.
read_fields(Fields, Bin) ->
    read_fields(Fields, Bin, []).
read_fields([], Bin, Rtn) ->
    {list_to_tuple(lists:reverse(Rtn)), Bin};
read_fields([{int8, _, _} | T], <<V:8/signed, Bin/binary>>, Rtn) ->
    read_fields(T, Bin, [V | Rtn]);
read_fields([{uint8, _, _} | T], <<V:8, Bin/binary>>, Rtn) ->
    read_fields(T, Bin, [V | Rtn]);
read_fields([{int16, _, _} | T], <<V:16/signed, Bin/binary>>, Rtn) ->
    read_fields(T, Bin, [V | Rtn]);
read_fields([{uint16, _, _} | T], <<V:16, Bin/binary>>, Rtn) ->
    read_fields(T, Bin, [V | Rtn]);
read_fields([{int32, _, _} | T], <<V:32/signed, Bin/binary>>, Rtn) ->
    read_fields(T, Bin, [V | Rtn]);
read_fields([{uint32, _, _} | T], <<V:32, Bin/binary>>, Rtn) ->
    read_fields(T, Bin, [V | Rtn]);
read_fields([{float, _, _} | T], <<V:64/float, Bin/binary>>, Rtn) ->
    read_fields(T, Bin, [V | Rtn]);
read_fields([{string, _, _} | T], <<Len:16, Bin/binary>>, Rtn) ->
    {V, B} = split_binary(Bin, Len),
    read_fields(T, B, [V | Rtn]);
read_fields([{byte, _, _} | T], <<Len:32, Bin/binary>>, Rtn) ->
    {V, B} = split_binary(Bin, Len),
    read_fields(T, B, [V | Rtn]);
read_fields([{array, _, _, Fields} | T], <<Len:16, Bin/binary>>, Rtn) ->
    {V, B} = read_array(Len, Bin, Fields, []),
    read_fields(T, B, [V | Rtn]).
%% 读取数组
read_array(0, Bin, _, Rtn) -> {lists:reverse(Rtn), Bin};
read_array(N, Bin, Fields, Rtn) ->
    {Val, B} = read_fields(Fields, Bin),
    read_array(N - 1, B, Fields, [Val | Rtn]).
