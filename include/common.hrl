%%----------------------------------------------------
%% 公共定义文件
%% (不要随意在此添加新的定义)
%%----------------------------------------------------
-define(DB, mysql_conn_pool).
-define(CLIENT_PC, 0).
-define(CLIENT_APK, 1).
-define(CLIENT_IPA, 2).

%% 数字型的bool值
-define(false, 0).
-define(true, 1).

%% bool值转数字型
-define(bool2int(V),
    case V of
        true -> 1;
        false -> 0
    end).
%% 数字型转bool值
-define(int2bool(V),
    case V of
        1 -> true;
        0 -> false
    end).

%% 语言翻译，返回给玩家的文本信息需要经过此宏的转换
-define(T(Text), lang:get(Text)).

%% 返回格式化字符串，等价于io_lib:format/2
-define(S(F, A), io_lib:format(F, A)).

%% 自定格式信息输出，相当于io:format，支持中文输出
-define(P(F, A),
    case os:type() of
        {win32, _} -> io:format(F, A);
        _ -> io:format("~ts", [io_lib:format(F, A)])
    end).
-define(P(F), ?P(F, [])).
%% 按固定格式输出调试信息，非debug模式下自动关闭
-ifdef(debug).
-define(DEBUG(Msg), logger:debug(Msg, [], ?MODULE, ?LINE)).
-define(DEBUG(F, A), logger:debug(F, A, ?MODULE, ?LINE)).
-else.
-define(DEBUG(Msg), ok).
-define(DEBUG(F, A), ok).
-endif.
%% 按固定格式输出普通信息到控制台
-define(INFO(Msg), logger:info(Msg, [], ?MODULE, ?LINE)).
-define(INFO(F, A), logger:info(F, A, ?MODULE, ?LINE)).
%% 按固定格式输出错误信息到控制台
-define(ERR(Msg), logger:error(Msg, [], ?MODULE, ?LINE)).
-define(ERR(F, A), logger:error(F, A, ?MODULE, ?LINE)).

%% 自定义类型:平台标识
-type p() :: bitstring().
%% 自定义类型:节点区号
-type z() :: non_neg_integer().
%% 自定义类型:角色ID
-type role_id() :: {pos_integer(), p(), z()}.
%% 自定义类型:角色帐号
-type role_acc() :: {bitstring(), p(), z()}.
%% 自定义类型:位置
-type pos() :: {pos_integer(), non_neg_integer(), non_neg_integer()}.
%% 自定义类型: MFA回调
-type m_f_a() :: {atom(), atom(), list()} | {function(), list()}.
%% 自定义类型:返回错误值
-type error_code() :: timeout %% 超时
    | noproc %% 进程不存在
    | exit %% 进程已退出
    | exist %% 已存在
    | not_found %% 未找到
    | not_exist %% 不存在
    | not_running %% 服务未运行
    | sql_error %% 数据库错误
    | role_data_convert_failure %% 数据转换出错
    | atom()
    | string() | bitstring() %% 返回消息
    .

%% 带catch的gen_server:call/2，返回{error, timeout} | {error, noproc} | {error, term()} | term() | {exit, normal}
%% 此宏只会返回简略信息，如果需要获得更详细的信息，请使用以下方式自行处理:
%% case catch gen_server:call(Pid, Request)
-define(CALL(_Call_Pid, _Call_Request),
    case catch gen_server:call(_Call_Pid, _Call_Request) of
        {'EXIT', {timeout, _}} -> {error, timeout};
        {'EXIT', {noproc, _}} -> {error, noproc};
        {'EXIT', {normal, _}} -> {error, exit};
        {'EXIT', _Call_Err} -> {error, _Call_Err};
        _Call_Return -> _Call_Return
    end
).

-define(CALL(_Call_Pid, _Call_Request, _CALL_TIMEOUT),
    case catch gen_server:call(_Call_Pid, _Call_Request, _CALL_TIMEOUT) of
        {'EXIT', {timeout, _}} -> {error, timeout};
        {'EXIT', {noproc, _}} -> {error, noproc};
        {'EXIT', {normal, _}} -> {error, exit};
        {'EXIT', _Call_Err} -> {error, _Call_Err};
        _Call_Return -> _Call_Return
    end
).

%% 未实现的函数请加入下面这个宏到函数体中
-define(NYI, io:format("*** NYI ~p ~p~n", [?MODULE, ?LINE]), exit(nyi)).

%% 将record转换成tuplelist
-define(record_to_tuplelist(Rec, Ref), lists:zip([record_name | record_info(fields, Rec)], tuple_to_list(Ref))).
%% 获取record的字段列表
-define(record_fields(Rec),  record_info(fields, Rec)).
%% 获取recrod字段所在位置
-define(record_field_pos(Rec, Field),  [K || K <- record_info(fields, Rec), K =:= Field]).
-define(record_fields_pos(Rec, Field),  [K || K <- record_info(fields, Rec), lists:member(K, Field)]).
