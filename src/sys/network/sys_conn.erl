%%----------------------------------------------------
%% 连接器 
%%----------------------------------------------------
-module(sys_conn).
-behaviour(gen_server).
-export([
        create/4
        ,send/2
        ,pack/2
        ,pack_send/3
        ,register_job/2
        ,register_trigger/3
    ]
).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
-include("common.hrl").
-include("conn.hrl").

-define(CONN_MAX_LEN, 524288).  %% 限制最大的单条协议长度
-define(CONN_MAX_BAD_REQ, 10).  %% 限制最大的错误请求数量
-define(CONN_MAX_BAD_SEND, 100). %% 限制最大的错误发送数量
-define(SYS_CONN_SOCKET_READ_TIMEOUT, 60000).

%% 调试选项
-ifdef(dbg_socket).
-define(PRINT_SEND(Bin),
    <<_L:32, C:16, B/binary>> = case is_list(Bin) of
        true -> list_to_binary(Bin);
        false -> Bin
    end,
    ?P("<SEND> [Code:~p Acc:~w Size:~w Data:~w]~n", [C, State#conn.bind_obj, byte_size(B), B])
).
-define(PRINT_RECV(Code, Data),
    ?P("<RECV> [Code:~w Acc:~w Size:~w Data:~w]~n", [Code, State#conn.bind_obj, byte_size(Data), Data])
).
-else.
-define(PRINT_SEND(Bin), ok).
-define(PRINT_RECV(Cmc, Data), ok).
-endif.

%% ----------------------------------------------------
%% 对外接口
%% ----------------------------------------------------

%% @doc 创建一个连接器
-spec create(ConnType, Socket, Ip, Port) -> {ok, pid()} | ignore | {error, term()} when
    ConnType :: gateway | game_server | monitor | tester,
    Socket :: port(),
    Ip :: tuple(),
    Port :: pos_integer().
create(ConnType, Socket, Ip, Port) ->
    gen_server:start(?MODULE, [ConnType, Socket, Ip, Port], []).

%% @doc 通知连接器发送数据
%% <div>当此函数在角色进程内执行时会自动处理发送缓冲区的操作</div>
%% <div>如果不是在角色进程内执行则会直接将数据发送到客户端</div>
-spec send(pid(), binary()) -> ok.
send(ConnPid, Bin) when is_pid(ConnPid) ->
    ConnPid ! {send_data, Bin},
    ok;
send(_, _) -> ok.

%% @doc 打包
pack(Code, Data) ->
    case sys_conn_cfg:module(Code) of
        {ok, _Auth, _Caller, Parser, _ModName} ->
            case catch Parser:pack(srv, Code, Data) of
                {ok, Bin} -> {ok, Bin};
                _Err ->
                    ?ERR("打包数据出错[Code: ~w][Err: ~w][Data: ~w]", [Code, _Err, Data]),
                    {error, pack}
            end;
        {error, _} ->
            ?ERR("模块影射失败[~w]:~w", [Code, Data]),
            {error, mapping}
    end.

%% @doc 打包并发送消息
-spec pack_send(pid(), pos_integer(), tuple()) -> ok.
pack_send(ConnPid, Code, Data) when is_pid(ConnPid) ->
    case pack(Code, Data) of
        {ok, Bin} -> send(ConnPid, Bin);
        _ -> ok
    end;
pack_send(_ConnPid, _Code, _Data) -> ok.

%% @doc 注册定时工作
register_job(ConnPid, Job) when is_pid(ConnPid) andalso is_record(Job, conn_job) ->
    ConnPid ! {register_job, Job},
    ok;
register_job(_, _) -> ok.

%% @doc 注册触发器
register_trigger(ConnPid, Type, MFA) when is_pid(ConnPid) ->
    ConnPid ! {register_trigger, Type, MFA},
    ok;
register_trigger(_, _, _) -> ok.
    
    

%% ----------------------------------------------------
%% 内部处理
%% ----------------------------------------------------

init([ConnType, Socket, Ip, Port]) ->
    process_flag(trap_exit, true),
    put(conn_jobs, []),
    put(triggers, []),
    Jobs = sys_conn_cfg:get_init_jobs(),
    lists:foreach(fun(Job) -> self() ! {register_job, Job} end, Jobs),
    self() ! read_next,
    State = #conn{type = ConnType, socket = Socket, ip = Ip, port = Port, connect_time = os:timestamp()},
    {ok, State}.

handle_call(_Request, _From, State) ->
    {noreply, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

%% 注册定时任务
handle_info({register_job, Job = #conn_job{interval = Interval, m = M, f = F, a = A, at_once = AtOnce}}, State) ->
    put(conn_jobs, [Job | get(conn_jobs)]),
    case AtOnce of
        true ->
            case erlang:apply(M, F, [State | A]) of
                ok ->
                    erlang:send_after(Interval, self(), {do_job, Job}),
                    {noreply, State};
                stop ->
                    {stop, normal, State}
            end;
        false ->
            erlang:send_after(Interval, self(), {do_job, Job}),
            {noreply, State}
end;

%% 执行定时任务
handle_info({do_job, Job = #conn_job{interval = Interval, m = M, f = F, a = A}}, State) ->
    case erlang:apply(M, F, [State | A]) of
        ok ->
            erlang:send_after(Interval, self(), {do_job, Job}),
            {noreply, State};
        {ok, NewState} when is_record(NewState, conn) ->
            erlang:send_after(Interval, self(), {do_job, Job}),
            {noreply, NewState};
        stop ->
            {stop, normal, State};
        {stop, Reason} ->
            {stop, Reason, State}
    end;

%% -------------------------------------------------------------------
%% socket 读取
%% -------------------------------------------------------------------
%% 继续接收数据
handle_info(read_next, State) ->
    read_next(State);

%% 客户端断开了连接
handle_info({inet_async, _Socket, _Ref, {error, closed}}, State = #conn{bind_obj = _BindObj}) ->
    ?DEBUG("[~w]断开了连接", [_BindObj]),
    {stop, normal, State};
%% 接收socket数据时发生了未预料的错误
handle_info({inet_async, _Socket, _Ref, {error, _Reason}}, State = #conn{bind_obj = _BindObj}) ->
    ?ERR("[~w]读取socket数据出错:~w", [_BindObj, _Reason]),
    {stop, normal, State};

%% 收到包头数据，检查长度，如果太长的数据则不接受
handle_info({inet_async, _Socket, _Ref, {ok, <<Len:32>>}}, State = #conn{bind_obj = _BindObj, read_head = true}) when Len > ?CONN_MAX_LEN ->
    ?ERR("[~w]读取的socket数据过长: ~w", [_BindObj, Len]),
    {stop, normal, State};
%% 收到包头数据，继续读取包体内容
handle_info({inet_async, Socket, _Ref, {ok, <<Len:32>>}}, State = #conn{read_head = true}) ->
    prim_inet:async_recv(Socket, Len, 100000),
    {noreply, State#conn{length = Len, read_head = false}};

%% 收到正常数据
handle_info({inet_async, Socket, _Ref, {ok, <<Code:16, Bin/binary>>}}, State = #conn{read_head = false, bad_req_count = BadReq}) ->
    ?PRINT_RECV(Code, Bin),
    case routing(Code, Bin, State) of
        ok ->
            {noreply, State};
        read_next ->
            read_next(State);
        {read_next, NewState} when is_record(NewState, conn) ->
            read_next(NewState);
        {read_next, {reply, Code, Reply}} ->
            %% pack_send(self(), Code, Reply),
            NewState = do_pack_send(State, Code, Reply),
            read_next(NewState);
        {read_next, {reply, Code, Reply}, NewState} when is_record(NewState, conn) ->
            %% pack_send(self(), Code, Reply),
            NewState1 = do_pack_send(NewState, Code, Reply),
            read_next(NewState1);
        {read_next, bad_req} ->
            read_next(State#conn{bad_req_count = BadReq + 1});
        {stop, Reason} ->
            {stop, Reason, State};
        {stop, Reason, {reply, Code, Reply}} ->
            {ok, ReplyBin} = pack(Code, Reply),
            catch gen_tcp:send(Socket, ReplyBin),
            timer:sleep(500),
            {stop, Reason, State}
    end;
%% 收到异常数据
handle_info({inet_async, _Socket, _Ref, {ok, _Bin}}, State = #conn{bind_obj = _BindObj, ip = Ip, bad_req_count = BadReq}) when BadReq > ?CONN_MAX_BAD_REQ ->
    ?ERR("[~w][IP:~w]收到过多异常数据，直接断开连接: ~w ~w", [_BindObj, Ip, BadReq, _Bin]),
    {stop, normal, State};
handle_info({inet_async, Socket, _Ref, {ok, Bin}}, State = #conn{bind_obj = _BindObj, ip = Ip, socket = Socket, bad_req_count = BadReq}) ->
    ?ERR("[~w][IP:~w]发送了无效请求: ~w", [_BindObj, Ip, Bin]),
    {noreply, State#conn{bad_req_count = BadReq + 1}};

%% -------------------------------------------------------------------
%% socket 发送
%% -------------------------------------------------------------------
%% 发送socket数据
%% handle_info({send_data, _IoData}, State = #conn{bind_obj = _BindObj, ip = Ip, bad_send_count = BadSend})  when BadSend > ?CONN_MAX_BAD_SEND ->
%%     ?ERR("[~w][IP:~w]发送失败次数过多，直接断开连接: ~w ~w", [_BindObj, Ip, BadSend]),
%%     {stop, normal, State};
handle_info({send_data, IoData}, State = #conn{bind_obj = _BindObj, socket = Socket, bad_send_count = BadSend}) ->
    case catch erlang:port_command(Socket, IoData) of
        true ->
            ?PRINT_SEND(IoData),
            {noreply, State};
        false ->
            ?ERR("[~w]执行port_command失败", [_BindObj]),
            {noreply, State#conn{bad_send_count = BadSend + 1}};
        Else ->
            ?ERR("[~w]发送socket数据失败:~w", [_BindObj, Else]),
            {noreply, State#conn{bad_send_count = BadSend + 1}}
    end;

%% 处理socket数据发送结果
handle_info({inet_reply, _Socket, ok}, State) ->
    {noreply, State};
handle_info({inet_reply, _Socket, {error, closed}}, State = #conn{bind_obj = _BindObj}) ->
    ?ERR("[~w]发送socket数据出错:closed", [_BindObj]),
    {stop, normal, State};
handle_info({inet_reply, _Socket, {error, timeout}}, State = #conn{bind_obj = _BindObj, bad_send_count = BadSend}) ->
    ?ERR("[~w]发送socket数据时发生超时错误: timeout", [_BindObj]),
    {noreply, State#conn{bad_send_count = BadSend + 1}};
handle_info({inet_reply, _Socket, _Else}, State = #conn{bind_obj = _BindObj}) ->
    ?ERR("[~w]发送socket数据时发生了未预料的错误: ~w", [_BindObj, _Else]),
    {stop, normal, State};

handle_info({'EXIT', _Pid, normal}, State) ->
    {stop, normal, State};

%% 处理关联进程异常退出
handle_info({'EXIT', Pid, fix_link}, State = #conn{bind_pid = BindPid}) when Pid=:=BindPid ->
    ?DEBUG("顶号退出"),
    {stop, normal, State};
handle_info({'EXIT', Pid, Why}, State = #conn{bind_obj = _BindObj, bind_pid = BindPid}) when Pid =:= BindPid ->
    ?ERR("[~w]的进程[~w]异常退出，连接关闭:~w, self:~w", [_BindObj, Pid, Why, self()]),
    {stop, normal, State};

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, #conn{ip = _Ip, bind_obj = _BindObj, socket = Socket}) ->
    gen_tcp:close(Socket),
    ?DEBUG("[~w]的连接进程[~w]已退出[~w]", [_BindObj, self(), _Reason]),
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% ----------------------------------------------------
%% 私有函数
%% ----------------------------------------------------
%% 通知连接器读取下一条指令
read_next(State = #conn{socket = Socket, recv_count = _RecvCount, read_head = false}) ->
    prim_inet:async_recv(Socket, 4, ?SYS_CONN_SOCKET_READ_TIMEOUT), %% 这里的超时时间需要允许移动端极端情况网络延时
    {noreply, State#conn{read_head = true}};
read_next(State) ->
    %% 上一个数据包还未读取完成，忽略掉
    {noreply, State}.

%% 打包并发送
do_pack_send(State = #conn{bind_obj = _BindObj, socket = Socket, bad_send_count = BadSend}, Code, Reply) ->
    {ok, Bin} = pack(Code, Reply),
    case catch erlang:port_command(Socket, Bin) of
        true ->
            ?PRINT_SEND(Bin),
            State;
        false ->
            ?ERR("[~w]执行port_command失败", [_BindObj]),
            State#conn{bad_send_count = BadSend + 1};
        Else ->
            ?ERR("[~w]发送socket数据失败:~w", [_BindObj, Else]),
            State#conn{bad_send_count = BadSend + 1}
    end.

%% 路由处理 -> ok | read_next | {read_next, #conn{}} | {read_next, Reply :: tuple()} | {read_next, bad_req} | {stop, Reason :: term()} | {stop, Reason :: term(), Reply :: tuple()}.
routing(Code, Bin, State = #conn{type = _Type, bind_obj = BindObj}) ->
    case sys_conn_cfg:module(Code) of
        {ok, true, Caller, Parser, Mod} when BindObj =/= <<>> ->
            call(Caller, Parser, Mod, Code, Bin, State);
        {ok, false, Caller, Parser, Mod} ->
            call(Caller, Parser, Mod, Code, Bin, State);
        _Else ->
            ?ERR("[~w]发送了无效请求[~w]:~w", [BindObj, Code, _Else]),
            {read_next, bad_req}
    end.

%% 由connector执行调用
call(connector, Parser, Mod, Code, Bin, State = #conn{bind_obj = BindObj}) ->
    case catch Parser:unpack(srv, Code, Bin) of
        {ok, Data} ->
            case catch Mod:handle(Code, Data, State) of
                ok -> read_next;
                {ok, NewState} when is_record(NewState, conn) ->
                    {read_next, NewState};
                {reply, Reply} ->
                    {read_next, {reply, Code, Reply}};
                {reply, Reply, NewState} when is_record(NewState, conn) ->
                    {read_next, {reply, Code, Reply}, NewState};
                {stop, reply, Reply} ->
                    {stop, normal, {reply, Code, Reply}};
                {stop, _Reason} ->
                    ?ERR("[~w]的连接器在处理命令时出错[Reason:~ts]", [BindObj, _Reason]),
                    {stop, normal};
                {error, _Reason} ->
                    ?ERR("[~w]的连接器在处理命令时出错[Reason:~w Code:~w Data:~w]", [BindObj, _Reason, Code, Data]),
                    read_next;
                _Reason ->
                    ?ERR("[~w]的连接器在处理命令时发生了未预料的错误[Mod:~w Code:~w Data:~w Reason:~w]", [BindObj, Mod, Code, Data, _Reason]),
                    read_next
            end;
        _Err ->
            ?ERR("解包数据出错[Mod:~w Code:~w Err:~w]:~w", [Mod, Code, _Err, Bin]),
            read_next
    end;

%% 过滤无效请求
call(object, _Parser, _Mod, _Code, _Data, #conn{bind_pid = undefined}) -> read_next;
call(object, _Parser, _Mod, _Code, _Data, #conn{object = undefined}) -> read_next;

%% 由object执行调用
call(object, Parser, Mod, Code, Bin, #conn{object = Object, bind_pid = BindPid}) ->
    Object:rpc(BindPid, Parser, Mod, Code, Bin),
    ok. %% 注意:此处不能直接read_next，必须等调用者通知后才读取下一个包，否则容易被攻击
