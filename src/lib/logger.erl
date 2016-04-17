%%----------------------------------------------------
%% 日志记录器
%%----------------------------------------------------
-module(logger).
-export([
        info/1
        ,info/2
        ,info/4
        ,debug/1
        ,debug/2
        ,debug/4
        ,error/1
        ,error/2
        ,error/4
        ,err_log/1
        ,err_log/2
        ,err_log/4
        ,format/5
    ]
).

%% @doc 输出系统信息到控制台
-spec info(iodata()) -> ok.
info(Msg) ->
    info(Msg, []).

%% @doc 输出系统信息到控制台，带格式化参数
-spec info(iodata(), list()) -> ok.
info(Format, Args) ->
    info(Format, Args, null, 0).

%% @doc 输出系统信息到控制台，带格式化参数，模块名和行号
-spec info(iodata(), list(), atom(), non_neg_integer()) -> ok.
info(Format, Args, Mod, Line) ->
    Msg = format("info", Format, Args, Mod, Line),
    io:format("~ts", [Msg]).

%% @doc 输出调试信息到控制台
-spec debug(iodata()) -> ok.
debug(Msg) ->
    debug(Msg, []).

%% @doc 输出调试信息到控制台，带格式化参数
-spec debug(iodata(), list()) -> ok.
debug(Format, Args) ->
    debug(Format, Args, null, 0).

%% @doc 输出调试信息到控制台，带格式化参数，带模块名和行号
-spec debug(iodata(), list(), atom(), non_neg_integer()) -> ok.
debug(Format, Args, Mod, Line) ->
    Msg = case Line of
        null -> io_lib:format(lists:concat(["## debug ", Format, "~n"]), Args);
        _ -> io_lib:format(lists:concat(["## debug [~w:~w] ", Format, "~n"]), [Mod, Line] ++ Args)
    end,
    io:format("~ts", [Msg]).
%% debug(Format, Args, Mod, Line) ->
%%     Msg = case Line of
%%         null -> erlang:iolist_to_binary(io_lib:format(lists:concat(["## debug ", Format, "~n"]), Args));
%%         _ -> erlang:iolist_to_binary(io_lib:format(lists:concat(["## debug [~w:~w] ", Format, "~n"]), [Mod, Line] ++ Args))
%%     end,
%%     io:format("~ts", [Msg]).

%% @doc 输出错误信息到控制台
-spec error(iodata()) -> ok.
error(Msg) ->
    ?MODULE:error(Msg, []).

%% @doc 输出错误信息到控制台，带格式化参数
-spec error(iodata(), list()) -> ok.
error(Format, Args) ->
    ?MODULE:error(Format, Args, null, 0).

%% @doc 输出错误信息到控制台，带格式化参数，带模块名和行号
-spec error(iodata(), list(), atom(), non_neg_integer()) -> ok.
error(Format, Args, Mod, Line) ->
    Msg = format("error", Format, Args, Mod, Line),
    io:format("~ts", [Msg]).

%% @doc 输出错误信息到控制台并记录日志
-spec err_log(iodata()) -> ok.
err_log(Msg) ->
    err_log(Msg, []).

%% @doc 输出错误信息到控制台并记录日志，带格式化参数
-spec err_log(iodata(), list()) -> ok.
err_log(Format, Args) ->
    err_log(Format, Args, null, 0).

%% @doc 输出错误信息到控制台并记录日志，带格式化参数，带模块名和行号
-spec err_log(iodata(), list(), atom(), non_neg_integer()) -> ok.
err_log(Format, Args, Mod, Line) ->
    Msg = format("elog", Format, Args, Mod, Line),
    io:format("~ts", [Msg]),
    log(Msg).

%% @doc 格式化日志信息
-spec format(string(), iodata(), list(), atom(), non_neg_integer()) -> iodata().
format(T, F, A, Mod, Line) ->
    {{Y, M, D}, {H, I, S}} = erlang:localtime(),
    Date = lists:concat([Y, "/", M, "/", D, " ", H, ":", I, ":", S]),
    case Line > 0 of
        false -> io_lib:format(lists:concat(["## ", T, " ~ts ", F, "~n"]), [Date] ++ A);
        true -> io_lib:format(lists:concat(["## ", T, " ~ts[~w:~w] ", F, "~n"]), [Date, Mod, Line] ++ A)
    end.
%% format(T, F, A, Mod, Line) ->
%%     {{Y, M, D}, {H, I, S}} = erlang:localtime(),
%%     Date = lists:concat([Y, "/", M, "/", D, " ", H, ":", I, ":", S]),
%%     case Line > 0 of
%%         false -> erlang:iolist_to_binary(io_lib:format(lists:concat(["## ", T, " ~ts ", F, "~n"]), [Date] ++ A));
%%         true -> erlang:iolist_to_binary(io_lib:format(lists:concat(["## ", T, " ~ts[~w:~w] ", F, "~n"]), [Date, Mod, Line] ++ A))
%%     end.

%% @doc 日志记录函数
-spec log(iodata()) -> ok.
log(Msg) ->
    db:exec("insert into log_error (msg) values(?)", [Msg]),
    ok.
