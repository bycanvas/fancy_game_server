%%----------------------------------------------------
%% 角色进程
%%----------------------------------------------------
-module(role).
-behaviour(gen_server).
-export([
        create/1, create/3
        ,stop/1, stop/3
        ,fix_link/4
        ,rpc/5
        ,pack_send/3
        ,apply/3
        ,convert/2
        ,element/2
    ]
).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-include("common.hrl").
-include("role.hrl").
-include("protocol.hrl").
-include("link.hrl").
-include("conn.hrl").
-include("pos.hrl").


%% ----------------------------------------------------
%% 对外接口
%% ----------------------------------------------------

%% @doc 创建一个角色进程
%% <div>创建的角色进程具有全服唯一的进程名称</div>
-spec create({Rid, Platform, ZoneId}, Account, Link) -> {ok, pid()} | ignore | {error, term()} when
    Rid :: integer(),
    Platform :: bitstring(),
    ZoneId :: non_neg_integer(),
    Account :: bitstring(),
    Link :: #link{}.
create({Rid, Platform, ZoneId}, Account, Link) ->
    gen_server:start({global, {role, Rid, Platform, ZoneId}}, ?MODULE, [Rid, Platform, ZoneId, Account, Link], []).

%% @doc 创建一个角色进程(用于玩家离线开启处理)
%% <div>创建的角色进程具有全服唯一的进程名称</div>
%% <div>定时关闭</div>
-spec create({Rid, Platform, ZoneId}) -> {ok, pid()} | ignore | {error, term()} when
    Rid :: integer(),
    Platform :: bitstring(),
    ZoneId :: non_neg_integer().
create({Rid, Platform, ZoneId}) ->
    gen_server:start({global, {role, Rid, Platform, ZoneId}}, ?MODULE, [Rid, Platform, ZoneId], []).

%% @doc 强制终止角色进程的运行
%% <div>
%% 可选择同步sync和异步async方式
%% 注意:如果Msg为非空则需要进程需要等Msg发送完成后才返回，当玩家网络不好时，这个时间可能会比较长
%% </div>
-spec stop(async | sync, pid(), bitstring()) -> ok | {error, term()}.
stop(async, RolePid, Msg) ->
    RolePid ! {stop, Msg},
    ok;
stop(sync, RolePid, Msg) ->
    ?CALL(RolePid, {stop, Msg}).

%% @doc 强制终止角色进程的运行
%% <div>系统节点关闭使用</div>
stop(RolePid) ->
    catch gen_server:call(RolePid, {stop, fast}, infinity).

%% @doc 处理顶号的连接器替换
-spec fix_link(Pid, RoleId, Account, NewLink) -> ok | {false, bitstring()} | {error, term()} when
    Pid :: pid(),
    RoleId :: role_id(),
    Account :: bitstring(),
    NewLink :: #link{}.
fix_link(Pid, RoleId, Account, NewLink) ->
    ?CALL(Pid, {fix_link, RoleId, Account, NewLink}).

%% @doc 客户端调用接口(socket事件处理)
-spec rpc(RolePid, Parser, Mod, Code, Bin) -> ok when
    RolePid :: pid(),
    Parser :: atom(),
    Mod :: atom(),
    Code :: pos_integer(),
    Bin :: binary().
rpc(RolePid, Parser, Mod, Code, Bin) ->
    RolePid ! {rpc, Parser, Mod, Code, Bin},
    ok.

%% @doc 对指定角色应用MFA(可选择同步或异步方式)
%% <div>注意:使用异步方式时，F的返回值格式要求: F([Role | A]) -> ok | {ok, NewRole}，且调用者只会收到ok的返回值</div>
%% <div>注意:使用同步方式时，F的返回值格式要求: F([Role | A]) -> {ok, Reply} | {ok, Reply, NewRole}，且调用者会收到F的执行结果Reply</div>
-spec apply(T, RolePid, Mfa) -> ok | {error, not_pid} | {error, self_call} | Reply when
    T :: async | sync,
    RolePid :: pid(),
    Mfa :: {F} | {F, A} | {M, F, A},
    M :: atom(),
    F :: atom() | function(),
    A :: list(),
    Reply :: term().
apply(async, RolePid, {F}) ->
    RolePid ! {apply_async, {F}},
    ok;
apply(async, RolePid, {F, A}) ->
    RolePid ! {apply_async, {F, A}},
    ok;
apply(async, RolePid, {M, F, A}) ->
    RolePid ! {apply_async, {M, F, A}},
    ok;
apply(sync, RolePid, _Mfa) when not is_pid(RolePid) ->
    {error, not_pid};
apply(sync, RolePid, Mfa) when self() =:= RolePid ->
    {M, F, _, _} = hd(tl(erlang:get_stacktrace())),
    ?ERR("调用者[~w:~w]执行apply[~w]时发生错误：调用了自身", [M, F, Mfa]),
    {error, self_call};
apply(sync, RolePid, {F}) ->
    ?CALL(RolePid, {apply_sync, F});
apply(sync, RolePid, {F, A}) ->
    ?CALL(RolePid, {apply_sync, {F, A}});
apply(sync, RolePid, {M, F, A}) ->
    ?CALL(RolePid, {apply_sync, {M, F, A}}).

%% @doc 将角色转换为指定的数据类型
%% <div>此接口在跨节点访问时可以节省大量的数据传输，非必要时不要跨节点传输整个#role{}数据</div>
-spec convert(RolePid, Type) -> {ok, tuple()} | {error, timeout} | {error, noproc} | {error, term()} when
    RolePid :: pid(),
    Type :: atom().
convert(RolePid, _Type) when self() =:= RolePid ->
    case erlang:get_stacktrace() of
        [] -> ?ERR("进程执行role:convert/2调用时发生错误: ~w", [self()]);
        L ->
            {M, F, _, _} = hd(tl(L)),
            ?ERR("调用者[~w:~w]执行role:convert/2时发生错误：调用了自身", [M, F])
    end,
    {error, self_call};
convert(RolePid, Type) ->
    ?CALL(RolePid, {convert, Type}).

%% @doc 获取指定角色的某些属性
%% <div>示例:[Event, Status] = role:element(RolePid, [#role.event, #role.status])</div>
-spec element(Role, ElemPos) -> {ok, [term()]} | {error, self_call} when
    Role :: pid() | #role{},
    ElemPos :: [pos_integer()].
element(Role = #role{}, ElemPos) when is_list(ElemPos) ->
    {ok, lists:reverse(do_element(ElemPos, Role, tuple_size(Role), []))};
element(RolePid, _ElemPos) when self() =:= RolePid ->
    {M, F, _, _} = hd(tl(erlang:get_stacktrace())),
    ?ERR("调用者[~w:~w]执行role:element/2时发生错误：调用了自身", [M, F]),
    {error, self_call};
element(RolePid, ElemPos) when is_list(ElemPos) ->
    ?CALL(RolePid, {element, ElemPos}).

%% @doc 打包并发送消息到角色进程(由角色进程转发到连接器进程)
%% <div>出于性能考虑，只能用于无法直接获取连接器Pid时使用</div>
-spec pack_send(RolePid, Code, Data) -> ok when
    RolePid :: pid(),
    Code :: pos_integer(),
    Data :: tuple().
pack_send(RolePid, Code, Data) when is_pid(RolePid) ->
    case sys_conn:pack(Code, Data) of
        {ok, Bin} ->
            RolePid ! {socket_proxy, Bin};
        _ -> ignore
    end,
    ok;
pack_send(_RolePid, _Code, _Data) ->
    ok.

%% ----------------------------------------------------
%% 内部处理
%% ----------------------------------------------------

%% 玩家登录
init([Rid, Platform, ZoneId, Account, Link = #link{conn_pid = ConnPid}]) ->
    put(is_role_process, true), %% 标识下这是一个角色进程
    put(already_handle_stop, false), %% 是否已经执行过退出处理
    put(conn_pid, ConnPid),
    put(is_online, true),   %% 是否真实在线
    case role_data:fetch(by_id, {Rid, Platform, ZoneId}) of
        {error, not_exist} -> {stop, not_exist};
        {error, role_data_invalid} -> {stop, role_data_invalid};
        {error, Reason} -> {stop, Reason};
        {ok, #role{account = Acc}} when Acc =/= Account -> {stop, not_owner};
        {ok, Role} ->
            put(role_info, {Rid, Platform, ZoneId, Account}), %% 将角色帐号信息放入进程字典
            process_flag(trap_exit, true),
            link(ConnPid),
            self() ! init,
            {ok, Role#role{loop_cnt = 1, need_sync = false, pid = self(), link = Link}}
    end.

%% 顶号处理
handle_call({fix_link, RoleId, Account, _NewLink}, _From, Role = #role{id = Id, account = Acc}) when Id =/= RoleId orelse Account =/= Acc ->
    ?ERR("玩家新登录接管玩家数据失败[RoleId:~w Id:~w Account:~w Acc:~w]", [RoleId, Id, Account, Acc]),
    {reply, {false, ?T("登录失败，错误的玩家ID")}, Role};
handle_call({fix_link, _RoleId, _Account, NewLink = #link{conn_pid = NewConnPid}}, _From, Role = #role{link = #link{conn_pid = ConnPid}}) when is_pid(ConnPid) ->
    ?DEBUG("玩家[Acc:~ts]已登录，接受顶号处理", [_Account]),
    erlang:unlink(ConnPid),
    erlang:link(NewConnPid),
    erlang:exit(ConnPid, fix_link),
    Role1 = Role#role{link = NewLink},
    NewRole = Role1,
    put(is_online, true),
    case get(ref_check_online) of
        undefined -> ignore;
        Ref ->
            erlang:cancel_timer(Ref),
            put(ref_check_online, undefined)
    end,
    {reply, ok, NewRole};

%% 执行同步apply操作
handle_call({apply_sync, {F}}, _From, Role) ->
    handle_apply_sync_return(catch erlang:apply(F, [Role]), {undefined, F, []}, Role);
handle_call({apply_sync, {F, A}}, _From, Role) ->
    handle_apply_sync_return(catch erlang:apply(F, [Role | A]), {undefined, F, A}, Role);
handle_call({apply_sync, {M, F, A}}, _From, Role) ->
    handle_apply_sync_return(catch erlang:apply(M, F, [Role | A]), {M, F, A}, Role);

handle_call({convert, Type}, _From, Role) ->
    {reply, role_convert:to(Type, Role), Role};

handle_call({element, Pos}, _From, Role) when is_list(Pos) ->
    {reply, ?MODULE:element(Role, Pos), Role};

%% 系统关机
handle_call({stop, fast}, _From, Role) ->
    {stop, normal, Role};
%% 将角色踢下线(同步操作)
handle_call({stop, _Msg}, _From, Role) ->
    {stop, normal, ok, Role};

handle_call(_Request, _From, Role) ->
    {noreply, Role}.

handle_cast(_Msg, Role) ->
    {noreply, Role}.

handle_info(init, Role) ->
    case role_login:do(Role) of
        {error, _Err} ->
            {stop, normal, Role};
        {ok, NewRole} ->
            case role_data:save(NewRole) of
                {error, _} ->
                    {stop, normal, NewRole};
                ok ->
                    role_query:sync_online_cache(NewRole),
                    self() ! loop,
                    {noreply, NewRole}
            end
    end;

%% 将角色踢下线(异步操作)
handle_info({stop, _Msg}, Role) ->
    {stop, normal, Role};

%% 内部循环
handle_info(loop, Role = #role{loop_cnt = C, need_sync = Sync, link = #link{conn_pid = ConnPid}}) ->
    %% 约每隔180秒执行一次GC
    %% TODO:这里考虑用hibernate吧，直接调用GC可能会影响性能
    case C rem 18 =:= 0 of
        false -> ignore;
        true -> garbage_collect()
    end,

    %% 约每隔10秒检查一次数据保存需求
    case Sync of
        false -> ignore;
        true ->
            role_data:save(Role)
            %% role_trigger:async_fire(self(), #evt_role_changed{})
    end,

    %% 约每隔2分种检查连接进程是否堆积过多的消息
    case C rem 12 =:= 0 of
        true when is_pid(ConnPid) ->
            case process_info(ConnPid, message_queue_len) of
                {_, QueueLen} when QueueLen > 10000 ->
                    ?DEBUG("连接进程堆积消息过多，断开连接"),
                    erlang:exit(ConnPid, kill);
                _ -> ignore
            end;
        _ ->
            ignore
    end,

    %% 10秒后进行下次循环
    erlang:send_after(10000, self(), loop),
    {noreply, Role#role{loop_cnt = C + 1, need_sync = false}};

%% 执行异步apply操作
handle_info({apply_async, {F}}, Role) ->
    handle_apply_async_return(catch erlang:apply(F, [Role]), {undefined, F, []}, Role);
handle_info({apply_async, {F, A}}, Role) ->
    handle_apply_async_return(catch erlang:apply(F, [Role | A]), {undefined, F, A}, Role);
handle_info({apply_async, {M, F, A}}, Role) ->
    handle_apply_async_return(catch erlang:apply(M, F, [Role | A]), {M, F, A}, Role);

%% 处理客户端调用
handle_info({rpc, Parser, Mod, Cmd, Bin}, Role = #role{link = #link{conn_pid = ConnPid}}) ->
    NewRole = handle_rpc({Parser, Mod, Cmd, Bin}, Role),
    put(rpc_code, undefined),
    read_next(ConnPid, NewRole);

%% 处理位置同步
handle_info({update_pos, X, Y, Dir}, Role = #role{pos = Pos}) ->
    {noreply, Role#role{pos = Pos#pos{x = X, y = Y, dir = Dir}}};

%% 转发数据到连接进程
handle_info({socket_proxy, Bin}, Role = #role{link = #link{conn_pid = ConnPid}}) ->
    sys_conn:send(ConnPid, Bin),
    {noreply, Role};

%% 连接器进程异常退出，协助处理收尾工作
handle_info({'EXIT', ConnPid, _Why}, Role = #role{link = #link{socket = Socket, conn_pid = ConnPid}}) ->
    ?DEBUG("玩家进程收到 连接器进程结束 信息"),
    ok = gen_tcp:close(Socket),
    put(is_online, false),
    Ref = erlang:send_after(5*1000, self(), check_online),
    put(ref_check_online, Ref),
    {noreply, Role};
handle_info({'EXIT', _ConnPid, _Why}, Role) ->
    ?ERR("玩家进程收到 连接器进程结束 信息异常：未识别的连接进程"),
    {noreply, Role};

handle_info(check_online, Role) ->
    case get(is_online) of
        false ->
            {stop, normal, Role};
        true ->
            case get(ref_check_online) of
                undefined -> ignore;
                Ref ->
                    erlang:cancel_timer(Ref),
                    put(ref_check_online, undefined)
            end,
            {noreply, Role}
    end;

handle_info(_Info, Role = #role{name = Name}) ->
    ?ERR("角色进程[~ts]收到未知消息: ~w", [Name, _Info]),
    {noreply, Role}.

terminate(_Reason, Role = #role{id = _Id, name = _Name}) ->
    NewRole = do_stop(Role),
    map:async_leave(NewRole),
    role_query:clean_online_cache(NewRole),
    role_data:save(NewRole),
    ?DEBUG("角色进程[ID:~w NAME:~ts]结束:~w", [_Id, _Name, _Reason]),
    ok.

code_change(_OldVsn, Role, _Extra) ->
    {ok, Role}.

%% ----------------------------------------------------
%% 私有函数
%% ----------------------------------------------------
%% 处理角色退出操作
do_stop(Role = #role{}) ->
    put(already_handle_stop, true),
    NewRole = role_logout:do(Role),
    NewRole.

%% 处理客户端调用
handle_rpc(
    {Parser, Mod, Code, Bin},
    Role = #role{id = _RoleId, name = Name, debug_rpc = _DebugRpc, link = #link{conn_pid = ConnPid}}
) ->
    put(rpc_code, Code), %% 记录下当前的命令号
    case catch Parser:unpack(srv, Code, Bin) of
        {ok, Data} ->
            case catch Mod:handle(Code, Data, Role) of
                %% 无需回应客户端，且角色数据不更新，也不同步到磁盘和缓存
                ok -> Role;
                %% 无需回应客户端，角色数据需更新，但不立即同步到磁盘
                {ok, NewRole} when is_record(NewRole, role) ->
                    NewRole#role{need_sync = true};
                %% 需回应客户端，但角色数据无需更新，也无需同步到磁盘和缓存
                {reply, Reply} ->
                    sys_conn:pack_send(ConnPid, Code, Reply),
                    Role;
                %% 需回应客户端，角色数据也需更新，但不立即同步到磁盘和缓存
                {reply, Reply, NewRole} when is_record(NewRole, role) ->
                    sys_conn:pack_send(ConnPid, Code, Reply),
                    NewRole#role{need_sync = true};
                %% 无需回应客户端，但角色数据需要更新，且需立即同步到磁盘和缓存
                {sync, NewRole} when is_record(NewRole, role) ->
                    role_data:save(NewRole),
                    NewRole;
                %% 需回应客户端，角色数据也需更新，且需立即同步到磁盘和缓存
                {sync, Reply, NewRole} when is_record(NewRole, role) ->
                    sys_conn:pack_send(ConnPid, Code, Reply),
                    role_data:save(NewRole),
                    NewRole;
                Err ->
                    ?ERR("[~ts]的角色进程处理命令[~w]时出错:~w", [Name, Code, Err]),
                    Role
            end;
        _Err ->
            ?DEBUG("解包数据出错[Code:~w Err:~w]", [Code, _Err]),
            Role
    end.

%% 通知连接器读取下一条指令
read_next(ConnPid, Role) ->
    ConnPid ! read_next,
    {noreply, Role}.

%% 处理异步apply的返回值
handle_apply_async_return({ok, NewRole}, _Mfa, _Role) when is_record(NewRole, role) ->
    {noreply, NewRole#role{need_sync = true}};
handle_apply_async_return(ok, _Mfa, Role) ->
    {noreply, Role};
handle_apply_async_return(Else, {M, F, A}, Role) ->
    ?ERR("角色[~ts]执行{~w, ~w, ~w}时得到错误的返回值格式:~w", [Role#role.name, M, F, A, Else]),
    {noreply, Role}.

%% 处理同步apply的返回值
handle_apply_sync_return({ok, Reply, NewRole}, _Mfa, _Role) when is_record(NewRole, role) ->
    {reply, Reply, NewRole};
handle_apply_sync_return({ok, Reply}, _Mfa, Role) ->
    {reply, Reply, Role};
handle_apply_sync_return(Else, {M, F, A}, Role) ->
    ?ERR("角色[~ts]同步执行{~w, ~w, ~w}时得到错误的返回值格式:~w", [Role#role.name, M, F, A, Else]),
    {reply, Else, Role}.

%% 获取#role{}中的某些属性
do_element([], _Role, _MaxPos, L) -> L;
do_element([H | T], Role, MaxPos, L) when H > 0 andalso H =< MaxPos  ->
    do_element(T, Role, MaxPos, [erlang:element(H, Role) | L]);
do_element([H | T], Role, MaxPos, L) ->
    ?ERR("传给role:element/2的参数不正确: ~w", [H]),
    do_element(T, Role, MaxPos, L).
