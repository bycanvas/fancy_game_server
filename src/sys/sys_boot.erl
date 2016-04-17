%%----------------------------------------------------
%% 系统启动程序
%%----------------------------------------------------
-module(sys_boot).
-behaviour(gen_server).
-export([start_link/1]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
-export([boot_times/0]).

-include("common.hrl").
-include("service.hrl").
-record(state, {}).

%%----------------------------------------------------
%% 对外接口
%%----------------------------------------------------

boot_times() ->
    env:get(sys_boot_times).

%% 启动
-spec start_link(atom()) -> {ok, pid()} | ignore | {error, term()}.
start_link(NodeType) ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [NodeType], []).

%%----------------------------------------------------
%% 内部实现
%%----------------------------------------------------

init([NodeType]) ->
    State = #state{},
    %% 初如化系统启动成功次数
    case env:get(sys_boot_times) of
        undefined -> env:save(sys_boot_times, 0);
        _ -> ignore
    end,
    self() ! {init, NodeType},
    {ok, State}.

handle_call(_Request, _From, State) ->
    {noreply, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info({init, NodeType}, State) ->
    %% 启动节点服务
    T0 = date:unixtime(),
    env:set(sup_name, sup_boot),
    %% 启动DB服务
    ok = db_mysql:init(),
    Result = case services:config(NodeType) of
        {ok, Services} ->
            start_services(Services);
        _ ->
            {error, services_cfg_undefined}
    end,
    case Result of
        ok ->
            env:save_counter(sys_boot_times, 1), %% 系统启动成功次数加一
            io:format("-------------------------------------------------------------~n"),
            ?P(">> 系统第[~w]次启动完成，用时 ~w 秒~n", [env:get(sys_boot_times), date:unixtime() - T0]);
        {error, Err} ->
            ?INFO("系统启动失败，原因: ~w.", [Err])
    end,
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

start_services([]) -> ok;
start_services([{Id, Args} | T]) ->
    case services:get(Id) of
        {error, undefined} -> {error, {service_undefined, Id}};
        {ok, #service{id = Id, mfa = {M, F, A}, restart = Restart, shutdown = Shutdown, type = Type}} ->
            case supervisor:start_child(sup_boot, {Id, {M, F, A ++ Args}, Restart, Shutdown, Type, [Id]}) of
                {ok, _Pid} -> start_services(T);
                {error, Reason} -> {error, {Id, Reason}}
            end
    end.
