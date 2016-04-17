%%----------------------------------------------------
%% 数据库访问接口封装
%%
%% 使用的数据库驱动是emysql，这里的对一些常用的数据库
%% 操作进行重新封装，忽略了一些不常使用到的返回信息
%% 以方便使用，如果你需要这些返回信息，可以直接使用emysql
%% 的接口，目前这里提供的接口都不支持同时处理多条查询或更新，比如:
%% "select * from role where name = 'xxx'; update role set login = 1"
%%----------------------------------------------------
-module(db_mysql).
-export(
    [
        init/0
        ,exec/1
        ,exec/2
        ,select_limit/3
        ,select_limit/4
        ,get_one/1
        ,get_one/2
        ,get_row/1
        ,get_row/2
        ,get_all/1
        ,get_all/2
        ,format_sql/2
        ,format_args/1
        ,format_args/2
    ]
).
-include("common.hrl").
-include("mysql.hrl").

%% @doc 数据库连接初始化
-spec init() -> ok.
init() ->
    case env:get(db_cfg) of
        [DbHost, DbPort, DbUser, DbPass, DbName, DbEncode, DbConnNumMin, DbConnNumMax] ->
            init(DbHost, DbPort, DbUser, DbPass, DbName, DbEncode, DbConnNumMin, DbConnNumMax);
        {DbHost, DbPort, DbUser, DbPass, DbName, DbEncode, DbConnNumMin, DbConnNumMax} ->
            init(DbHost, DbPort, DbUser, DbPass, DbName, DbEncode, DbConnNumMin, DbConnNumMax);
        _Arg ->
            ?DEBUG("参数db_cfg忽略: ~w", [_Arg]),
            ok
    end.
init(DbHost, DbPort, DbUser, DbPass, DbName, DbEncode, DbConnNumMin, DbConnNumMax) ->
    emysql:add_pool(?DB, DbConnNumMin, DbConnNumMax, DbUser, DbPass, DbHost, DbPort, DbName, DbEncode).

%% @doc 执行一条数据库语句，注意：不能用这个接口执行查询类的语句
-spec exec(bitstring() | string()) -> {ok, non_neg_integer()} | {error, term()}.
exec(Sql) ->
    case catch emysql:execute(?DB, Sql) of
        #ok_packet{affected_rows = AffectRows} -> {ok, AffectRows};
        #error_packet{msg = Msg} -> {error, Msg};
        Err -> {error, Err}
    end.

%% @doc 执行一条数据库语句，带参数，注意：不能用这个接口执行查询类的语句
-spec exec(bitstring() | string(), [term()]) -> {ok, non_neg_integer()} | {error, term()}.
exec(Sql, Args) ->
    case catch emysql:execute(?DB, Sql, Args) of
        #ok_packet{affected_rows = AffectRows} -> {ok, AffectRows};
        #error_packet{msg = Msg} -> {error, Msg};
        Err -> {error, Err}
    end.

%% @doc 执行分页查询，返回结果中的所有行
-spec select_limit(bitstring() | string(), non_neg_integer(), pos_integer()) ->
    {ok, [term()]} | {error, term()}.
select_limit(Sql, Offset, Num) ->
    S = unicode:characters_to_binary([Sql, <<" limit ">>, integer_to_list(Offset), <<", ">>, integer_to_list(Num)]),
    case catch emysql:execute(?DB, S) of
        #result_packet{rows = Rows} -> {ok, Rows};
        #error_packet{msg = Msg} -> {error, Msg};
        Err -> {error, Err}
    end.

%% @doc 执行分页查询(带格式化参数)，返回结果中的所有行
-spec select_limit(bitstring() | string(), [term()], non_neg_integer(), pos_integer()) ->
    {ok, [term()]} | {error, term()}.
select_limit(Sql, Args, Offset, Num) ->
    S = unicode:characters_to_binary([Sql, " limit ", integer_to_list(Offset), ", ", integer_to_list(Num)]),
    case catch emysql:execute(?DB, S, Args) of
        #result_packet{rows = Rows} -> {ok, Rows};
        #error_packet{msg = Msg} -> {error, Msg};
        Err -> {error, Err}
    end.

%% @doc 取出查询结果中的第一行第一列(不带格式化参数)
%% <div>注意：必须确保返回结果中不会多于一行一列，其它情况或未找到时返回{error, undefined}</div>
-spec get_one(bitstring() | string()) -> {ok, term()} | {error, term()}.
get_one(Sql) ->
    case catch emysql:execute(?DB, Sql) of
        #result_packet{rows = []} -> {ok, undefined};
        #result_packet{rows = [[R]]} -> {ok, R};
        #error_packet{msg = Msg} -> {error, Msg};
        Err -> {error, Err}
    end.

%% @doc 取出查询结果中的第一行第一列(带有格式化参数)
%% <div>注意：必须确保返回结果中不会多于一行一列，其它情况或未找到时返回{error, undefined}</div>
-spec get_one(bitstring() | string(), [term()]) -> {ok, term()} | {error, term()}.
get_one(Sql, Args) ->
    case catch emysql:execute(?DB, Sql, Args) of
        #result_packet{rows = [[R]]} -> {ok, R};
        #result_packet{rows = []} -> {ok, undefined};
        #error_packet{msg = Msg} -> {error, Msg};
        Err -> {error, Err}
    end.

%% @doc 取出查询结果中的第一行
%% <div>注意：必须确保返回结果中不会多于一行，其它情况或未找到时返回{ok, undefined}</div>
-spec get_row(bitstring() | string()) -> {ok, [term()]} | {error, term()}.
get_row(Sql) ->
    case catch emysql:execute(?DB, Sql) of
        #result_packet{rows = [R]} -> {ok, R};
        #result_packet{rows = []} -> {ok, undefined};
        #error_packet{msg = Msg} -> {error, Msg};
        Err -> {error, Err}
    end.

%% @doc 取出查询结果中的第一行(带有格式化参数)
%% <div>注意：必须确保返回结果中不会多于一行，其它情况或未找到时返回{ok, undefined}</div>
-spec get_row(bitstring() | string(), [term()]) -> {ok, [term()]} | {error, term()}.
get_row(Sql, Args) ->
    case catch emysql:execute(?DB, Sql, Args) of
        #result_packet{rows = [R]} -> {ok, R};
        #result_packet{rows = []} -> {ok, undefined};
        #error_packet{msg = Msg} -> {error, Msg};
        Err -> {error, Err}
    end.

%% @doc 取出查询结果中的所有行
-spec get_all(bitstring() | string()) -> {ok, term()} | {error, term()}.
get_all(Sql) ->
    case catch emysql:execute(?DB, Sql) of
        #result_packet{rows = R} -> {ok, R};
        #error_packet{msg = Msg} -> {error, Msg};
        Err -> {error, Err}
    end.

%% @doc 取出查询结果中的所有行
-spec get_all(bitstring() | string(), [term()]) -> {ok, term()} | {error, term()}.
get_all(Sql, Args) ->
    case catch emysql:execute(?DB, Sql, Args) of
        #result_packet{rows = R} -> {ok, R};
        #error_packet{msg = Msg} -> {error, Msg};
        Err -> {error, Err}
    end.

%% @doc 组合sql语句
-spec format_sql(Sql::string(), Arg::list()) -> bitstring().
format_sql(Sql, Arg) ->
    As = io_lib:format(Sql, [emysql_util:encode(A, binary, utf8) || A <- Arg]),
    unicode:characters_to_binary(As).

%% @doc 合并多组SQL语句的参数
-spec format_args(Args::list()) -> bitstring().
format_args([]) -> <<>>;
format_args(Args = [H | _]) ->
    Len = length(H),
    S = gen_s(Len),
    format_args(S, Args).

%% @doc 按照给定的格式化参数列表，合并多组SQL语句参数
-spec format_args(Sarg::string(), Args::list()) -> bitstring().
format_args(S, [Arg]) ->
    As = io_lib:format(S, [emysql_util:encode(A, binary, utf8) || A <- Arg]),
    unicode:characters_to_binary(As);
format_args(S, Args) ->
    %% As = lists:map(fun(Arg) ->
    %%             io_lib:format(S, [emysql_util:encode(A, binary, latin1) || A <- Arg])
    %%     end, Args),
    %% list_to_bitstring(string:join(As, ",")).
    %% R17版本utf8编码问题
    As = lists:map(fun(Arg) ->
                io_lib:format(S, [emysql_util:encode(A, binary, utf8) || A <- Arg])
        end, Args),
    unicode:characters_to_binary(string:join(As, ",")).

%% ----------------------------------------------------
%% 内部方法
%% ----------------------------------------------------

%% 生成一定数量(~s,~s, ...)格式的格式化语句
gen_s(Len) ->
    Sl = lists:duplicate(Len, "~ts"),
    lists:concat([" (", string:join(Sl, ","), ")"]).
