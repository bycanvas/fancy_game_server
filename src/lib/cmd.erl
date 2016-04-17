%%----------------------------------------------------
%% 操作系统命令调用接口
%%----------------------------------------------------
-module(cmd).
-export([
        run/2
        ,run/3
    ]
).
-define(TIMEOUT, 180000). %% 执行超时时间设定，单位:毫秒

%% @doc 执行外部命令，参数说明参见
%% @see run/3
-spec run(atom() | string(), list()) -> {ok, term()} | {pos_integer(), term()}.
run(Cmd, Args) ->
    run(Cmd, Args, ?TIMEOUT).

%% @doc 执行外部命令，返回{ok, term()}表示调用正常结束，注意默认的执行超时为180秒
-spec run(Cmd, Args, Timeout) -> {ok, term()} | {255, exec_timeout} | {ExitStatus, Reason} when
    Cmd :: atom() | string(),
    Args :: list(),
    Timeout :: pos_integer(),
    ExitStatus :: pos_integer(),
    Reason :: term().
run(Cmd, Args, Timeout) ->
    Tag = make_ref(), 
    {Pid, Ref} = erlang:spawn_monitor(
        fun() ->
                Recv = try
                    case os:find_executable(Cmd) of
                        false -> 
                            S = io_lib:format("'~ts'不是可执行命令~n", [Cmd]),
                            {255, S};
                        C ->
                            P = open_port({spawn_executable, C}, [binary, use_stdio, stderr_to_stdout, exit_status, stream, eof, {args, Args}]),
                            recv(P, [], 255)
                    end
                catch
                    _T:X ->
                        {255, X}
                end,
                exit({Tag, Recv})
        end
    ),
    receive
        {'DOWN', Ref, process, Pid, {Tag, Data}} -> Data;
        {'DOWN', Ref, process, Pid, Reason} -> exit(Reason)
    after 
        Timeout ->
            exit(Pid, kill),
            {255, exec_timeout}
    end.

%% 接收外部命令返回的结果
recv(Port, Acc, Status) ->
    receive
        {Port, {data, Data}} ->
            recv(Port, [Data | Acc], Status);
        {Port, {exit_status, 0}} -> recv(Port, Acc, ok);
        {Port, {exit_status, Status}} -> recv(Port, Acc, Status);
        {Port, eof} -> {Status, lists:reverse(Acc)}
    end.
