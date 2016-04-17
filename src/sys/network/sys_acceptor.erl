%%----------------------------------------------------
%% TCP Acceptor
%%----------------------------------------------------
-module(sys_acceptor).

-behaviour(gen_server).
-export([start_link/1]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
-include("common.hrl").

%%----------------------------------------------------
%% 对外接口
%%----------------------------------------------------

%% @doc 启动acceptor
-spec start_link(port()) -> ignore | {ok, pid()} | {error, term()}.
start_link(LSock) ->
    gen_server:start_link(?MODULE, [LSock], []).

%%----------------------------------------------------
%% 内部处理
%%----------------------------------------------------

init([LSock]) ->
	self() ! loop,
    %% ?INFO("[~w] 已启动...", [?MODULE]),
    {ok, {env:get(node_type), LSock}}.

handle_call(_Request, _From, State) ->
    {noreply, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(loop, State = {NodeType, LSock}) ->
    case gen_tcp:accept(LSock) of
        {ok, Socket} ->
            ?DEBUG("监听到新连接:~w", [Socket]),
            gen_tcp:controlling_process(Socket, spawn(fun() -> accept(NodeType, Socket) end));
        {error, closed} ->
            ignore;
        {error, emfile} ->%% 超过单进程链接数上限
            util:sleep(5000),
            ?ERR("接受socket连接时发生了未预料的错误:emfile");
        {error, Reason} ->
            ?ERR("接受socket连接时发生了未预料的错误:~w", [Reason])
    end,
    self() ! loop, %% 等待下一个
    {noreply, State};

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%----------------------------------------------------
%% 私有函数
%%----------------------------------------------------

%% 接受一个连接
accept(_NodeType, Socket) ->
    create_conn(game_server, 4, Socket).

%% accept(_NodeType, Socket) ->
%%     case gen_tcp:recv(Socket, 4, 10000) of
%%         {ok, <<Len:32>>} ->
%%             ?DEBUG("11111111111111:len=~w", [Len]),
%%             case gen_tcp:recv(Socket, Len, 10000) of
%%                 {ok, <<Code:16, Bin/binary>>} ->
%%                     ?DEBUG("2222222222:Code=~w, Bin=~w", [Code, Bin]);
%%                 _Else ->
%%                     ?DEBUG("3333333333:~w", [_Else])
%%             end;
%%         _Other ->
%%             ?DEBUG("444444444:~w", [_Other])
%%     end,
%%     gen_tcp:close(Socket),
%%     ok.

%% 创建连接进程
create_conn(ConnType, _Packet, Socket) ->
    try
        {ok, {Ip, Port}} = inet:peername(Socket),
        ?DEBUG("监听到新连接IP:~w", [Ip]),
        %% ok = inet:setopts(Socket, env:get(tcp_options)),
        {ok, Pid} = sys_conn:create(ConnType, Socket, Ip, Port),
        %% ?DEBUG("成功建立一个新连接(类型:~p)", [ConnType])
        ok = gen_tcp:controlling_process(Socket, Pid)
    catch
        T:X ->
            ?ERR("创建连接失败[~w : ~w]", [T, X]),
            gen_tcp:close(Socket)
    end.
