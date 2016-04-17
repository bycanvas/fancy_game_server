%%----------------------------------------------------
%% 测试器进程
%%----------------------------------------------------
-module(tester).
-behaviour(gen_server).
-export([
        start/3
        ,info/1
        ,cmd/3
        ,cmd/4
        ,pack_send/2
    ]
).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
-include("common.hrl").
-include("tester.hrl").
-include("protocol.hrl").
-include("map.hrl").

-define(TCP_OPTS, [binary, {packet, 0}, {active, false}, {reuseaddr, true}, {nodelay, true}]).

%% 调试选项
-ifdef(dbg_socket).
-define(PRINT_SEND(Bin), ok).
-define(PRINT_RECV(Cmc, Data), ok).
-else.
-define(PRINT_SEND(Bin), ok).
-define(PRINT_RECV(Cmc, Data), ok).
-endif.

%% ----------------------------------------------------
%% 对外接口
%% ----------------------------------------------------

%% @doc 创建测试机器人进程，角色模拟进程
-spec start(N, Host, Port) -> {ok, pid()} | ignore | {error, term()} when
      N :: non_neg_integer(),
      Host :: string(),
      Port :: pos_integer().
start(N, Host, Port) ->
    gen_server:start(?MODULE, [N, Host, Port], []).

%% @doc 查询信息
info(Tester = #tester{}) ->
    L = ?record_to_tuplelist(tester, Tester),
    lists:foreach(fun({K, V}) ->
                          if
                              is_bitstring(V) orelse is_list(V) -> ?P("~w : ~ts~n", [K, V]);
                              true -> ?P("~w : ~w~n", [K, V])
                          end
                  end, L),
    ok;
info(N) ->
    case ets:lookup(tester_online, N) of
        [T] -> info(T);
        _ -> ?ERR("未找到[~w]的测试角色", [N])
    end.

%% @doc 执行模块命令(当前进程)
-spec cmd(atom(), pos_integer() | atom(), [term()]) -> ok.
cmd(Mod, Code, Data) ->
    cmd(self(), Mod, Code, Data).

%% @doc 执行模块命令(指定角色或pid)
-spec cmd(Id, Mod, Code, Data) -> ok | ignore when
    Id :: pid() | role_id(),
    Mod :: atom(),
    Code :: pos_integer() | atom(),
    Data :: [term()].
cmd(Pid, Mod, Cmd, Data) when is_pid(Pid) ->
    Pid ! {cmd, Mod, Cmd, Data},
    ok;
cmd(Id, Mod, Cmd, Data) ->
    case ets:lookup(tester_online, Id) of
        [#tester{pid = Pid}] ->
            cmd(Pid, Mod, Cmd, Data);
        _ ->
            ?T_DBG("角色[~w]为登陆系统，无法执行命令", [Id]),
            ok
    end.

%% @doc 打包并发送消息到服务器
-spec pack_send(pos_integer(), tuple()) -> ok.
pack_send(Code, Data) ->
    Socket = get(socket),
    case sys_conn_cfg:module(tester, Code) of
        {ok, Parser, _Mod} ->
            case Parser:pack(cli, Code, Data) of
                {ok, Bin} -> %% 注意这里的Bin是个iolist类型
                    ?PRINT_SEND(Bin),
                    put(send_proto, get(send_proto) + 1),
                    gen_tcp:send(Socket, Bin);
                Err ->
                    ?ERR("打包数据出错[Reason:~w]", [Err])
            end;
        {error, Reason} ->
            ?ERR("模块映射失败:~p", [Reason])
    end.

%% ----------------------------------------------------
%% 内部调用处理
%% ----------------------------------------------------

%% 网关连接器
init([N, Host, Port]) ->
    Platform = env:get(platform),
    ZoneId = 1,
    Account = util:fbin(<<"robot_~ts_~w_~w">>, [Platform, ZoneId, N]),
    Name = util:fbin(<<"robot_name_~w">>, [N]),
    %% 注册一个名字，方便查看信息
    erlang:register(list_to_atom(binary_to_list(Account)), self()),
    State = #tester{
               id = 0
               ,type = robot
               ,account = Account
               ,name = Name
               ,platform = Platform
               ,zone_id = ZoneId
               ,host = Host
               ,port = Port
               ,pid = self()
               ,object = gameserver
              },
    put(rid, 0),
    put(send_proto, 0),
    put(read_proto, 0),
    put(start_time, date:unixtime()),
    put(is_moving, false),
    self() ! init,
    {ok, State}.

handle_call(_Request, _From, State) ->
    {noreply, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(init, State = #tester{name = _Name, host = Host, port = Port, account = Account, platform = Platform, zone_id = ZoneId}) ->
    ?T_DBG("[~ts]开始连接服务器", [_Name]),
    {ok, Socket} = retry_conn(Host, Port, ?TCP_OPTS, 5),
    ok = gen_tcp:controlling_process(Socket, self()),
    put(socket, Socket),
    ?T_DBG("[~ts]成功连接上服务器", [_Name]),
    State1 = State#tester{socket = Socket},
    erlang:send_after(20000, self(), login_check), %% 检测登录超时
    cmd(test_client_open_rpc, login, [Account, Platform, ZoneId]),
    {noreply, State1};

%% 如果检查到帐号未登录则退出进程
handle_info(login_check, State = #tester{id = RoleId, account = _Account})->
    case RoleId =:= 0 of
        true ->
            ?ERR("账号[~ts]登录超时，进程结束", [_Account]),
            {stop, normal, State};
        false ->
            {noreply, State}
    end;

%% 成功连接上区服
handle_info(connected_zone, State) ->
    %% cmd(test_client_rpc, role_list, []),
    %% self() ! heartbeat,
    {noreply, State};

%% 心跳检测
handle_info(heartbeat, State = #tester{name = _Name}) ->
    erlang:send_after(30000, self(), heartbeat),
    Now = date:unixtime(),
    pack_send(1199, {}),
    case get(heartbeat_time) of
        T when is_integer(T) andalso Now - T > 60 ->
            ?ERR("角色[~ts]接收到心跳包延迟太久:~w", [_Name, Now - T]);
        _ -> ignore
    end,
    put(heartbeat_time, Now),
    {noreply, State};

%% 退出
handle_info(logout, State = #tester{id = RoleId}) ->
    ets:delete(tester_online, RoleId),
    {stop, normal, State};

%% 角色登录后处理
handle_info(login_role_done, State) ->
    ets:insert(tester_online, State),
    cmd(test_role_rpc, role_init, []),
    {noreply, State};

%% 地图处理完毕
handle_info(map_init_done, State) ->
    ?T_DBG("进入地图成功，开始AI"),
    self() ! ai_begin,
    {noreply, State};

%% AI阶段
handle_info(ai_begin, State) ->
    case get(is_moving) of
        true -> ignore;
        false ->
            test_map_rpc:move_rand(State)
    end,
    erlang:send_after(util:rand(1000, 3000), self(), ai_begin),
    {noreply, State};

%% 移动检测
handle_info({move_check, PerTime}, State = #tester{x = X, y = Y, speed = Speed}) ->
    MoveDist = round(Speed * PerTime / 1000),
    State1 = case get(move_dest) of
        {_DestX, DestY, ?MAP_ROLE_DIR_UP} ->
            Y1 = Y + MoveDist,
            case Y1 >= DestY of
                true ->
                    put(is_moving, false),
                    State#tester{y = DestY};
                false ->
                    State#tester{y = Y1}
            end;
        {_DestX, DestY, ?MAP_ROLE_DIR_DOWN} ->
            Y1 = Y - MoveDist,
            case Y1 =< DestY of
                true ->
                    put(is_moving, false),
                    State#tester{y = DestY};
                false ->
                    State#tester{y = Y1}
            end;
        {DestX, _DestY, ?MAP_ROLE_DIR_RIGHT} ->
            X1 = X + MoveDist,
            case X1 >= DestX of
                true ->
                    put(is_moving, false),
                    State#tester{x = DestX};
                false ->
                    State#tester{x = X1}
            end;
        {DestX, _DestY, ?MAP_ROLE_DIR_LEFT} ->
            X1 = X - MoveDist,
            case X1 =< DestX of
                true ->
                    put(is_moving, false),
                    State#tester{x = DestX};
                false ->
                    State#tester{x = X1}
            end
    end,
    test_map_rpc:move_arrive(State1),
    case get(is_moving) of
        true ->
            erlang:send_after(PerTime, self(), {move_check, PerTime});
        false ->
            ignore
    end,
    {noreply, State1};

%% 处理原生命令
handle_info({cmd, Mod, Code, Data}, State) ->
    %% ?INFO("请求处理命令Mod: ~w Code:~w", [Mod, Code]),
    call(Mod, Code, Data, State);

handle_info(stop, State) ->
    {stop, normal, State};

%% 服务端断开了连接
handle_info({inet_async, _Socket, _Ref, {error, closed}}, State = #tester{account = Account}) ->
    ?ERR("账号[~ts]连接关闭: ~w", [Account, _Socket]),
    {stop, normal, State};
%% 接收socket数据时发生了未预料的错误
handle_info({inet_async, _Socket, _Ref, {error, _Reason}}, State = #tester{account = Account}) ->
    ?ERR("帐号[~ts]连接读取socket数据出错:~w", [Account, _Reason]),
    {stop, normal, State};

%% 收到包头数据
handle_info({inet_async, Socket, _Ref, {ok, <<Len:32>>}}, State = #tester{read_head = true}) ->
    %% ?T_DBG("包头长度:~w", [Len]),
    prim_inet:async_recv(Socket, Len, -1), %% 读取内容
    {noreply, State#tester{read_head = false}};
%% 收到正常数据
handle_info({inet_async, _Socket, _Ref, {ok, <<Code:16, Bin/binary>>}}, State = #tester{account = Acc, read_head = false}) ->
    %% ?T_DBG("消息内容:Code=~w, Bin=~w", [Code, Bin]),
    put(read_proto, get(read_proto) + 1),
    case sys_conn_cfg:module(tester, Code) of
        {ok, Parser, Mod} ->
            case catch Parser:unpack(cli, Code, Bin) of
                {ok, Data} ->
                    call(Mod, Code, Data, State);
                _Err ->
                    %% ?ERR("解包数据出错: ~w", [Err]),
                    read_next(State)
                    %% {stop, normal, State}
            end;
        _Other ->
            ?ERR("账号[~ts]收到未知命令: ~w", [Acc, Code]),
            read_next(State)
    end;

handle_info(_Info, State) ->
    ?T_DBG("收到未知消息: ~w", [_Info]),
    {noreply, State}.

terminate(_Reason, _Tester = #tester{account = Account, name = _Name}) ->
    T = date:unixtime() - get(start_time),
    ?INFO("帐号[~ts]已经退出，本次在线时长:~w 发包数:~w 收包数:~w", [Account, T, get(send_proto), get(read_proto)]),
    case get(socket) of
        Socket when is_port(Socket) ->
            gen_tcp:close(Socket);
        _ -> ignore
    end,
    ok.

code_change(_OldVsn, State = #tester{}, _Extra) ->
    {ok, State}.

%% ----------------------------------------------------
%% 私有函数
%% ----------------------------------------------------
retry_conn(_Host, _Port, _Opts, 0) -> retry_failed;
retry_conn(Host, Port, Opts, N) ->
    case gen_tcp:connect(Host, Port, ?TCP_OPTS) of
        {ok, Socket} -> {ok, Socket};
        {error, econnreset} ->
            retry_conn(Host, Port, Opts, N-1);
        Err ->
            Err
    end.

%% 调用模块
call(Mod, Code, Data, State = #tester{account = Account, name = Name}) ->
    case catch Mod:handle(Code, Data, State) of
        ok ->
            read_next(State);
        {ok, NewState} when is_record(NewState, tester) ->
            read_next(NewState);
        stop ->
            {stop, normal, State};
        {error, _Reason} ->
            ?ERR("处理命令时出错[Account:~ts Name:~ts Mod:~w Code:~w Why:~w]: ~w", [Account, Name, Mod, Code, _Reason, Data]),
            read_next(State);
        _Reason ->
            ?ERR("处理命令时发生了未预料的错误[Account:~ts Name:~ts Mod:~w Code:~w]: ~w", [Account, Name, Mod, Code, _Reason]),
            read_next(State)
    end.

%% 通知连接器读取下一条指令
read_next(State = #tester{socket = Socket, read_head = false}) ->
    prim_inet:async_recv(Socket, 4, -1),
    {noreply, State#tester{read_head = true}};
%% 上一个数据包还未读取完成，忽略掉
read_next(State) ->
    {noreply, State}.
