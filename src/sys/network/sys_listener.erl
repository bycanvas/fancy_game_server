%%----------------------------------------------------
%% TCP监听处理
%%----------------------------------------------------
-module(sys_listener).
-behaviour(gen_server).
-export([start_link/0, stop/0]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-include("common.hrl").

%%----------------------------------------------------
%% 对外接口
%%----------------------------------------------------

%% @doc 开启连接监听服务
-spec start_link() -> {ok, pid()} | ignore | {error, term()}.
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

%% @doc 关闭连接监听服务
-spec stop() -> ok.
stop() ->
    ?INFO("[~w] 正在关闭...", [?MODULE]),
    supervisor:terminate_child(env:get(sup_name), sup_acceptor),
    supervisor:terminate_child(env:get(sup_name), sys_listener),
    ?INFO("[~w] 已经关闭...", [?MODULE]),
    ok.

%%----------------------------------------------------
%% 内部处理
%%----------------------------------------------------

init([]) ->
    ?INFO("[~w] 正在启动...", [?MODULE]),
    Port = env:get(port),
    case gen_tcp:listen(Port, env:get(tcp_options)) of
        {ok, LSock} ->
            N = env:get(tcp_acceptor_num),
            ?INFO("[~w] 成功监听到端口:~w", [?MODULE, Port]),
            ?INFO("启动acceptor数量:~w", [N]),
            start_acceptor(N, LSock),
            ?INFO("[~w] 启动完成", [?MODULE]),
            {ok, state};
        {error, Reason}->
            ?ERR("[~w] 无法监听到~w:~w", [?MODULE, Port, Reason]),
            {stop, listen_failure, state}
    end.

handle_call(_Request, _From, State) ->
    {noreply, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ?INFO("[~w] 正在关闭...", [?MODULE]),
    ?INFO("[~w] 关闭完成", [?MODULE]),
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%----------------------------------------------------
%% 私有函数
%%----------------------------------------------------

start_acceptor(0, _LSock)-> ok;
start_acceptor(N, LSock)->
    {ok, Pid} = supervisor:start_child(sup_acceptor, [LSock]),
    Pid ! {event, start},
    start_acceptor(N - 1, LSock).
