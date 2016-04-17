%%----------------------------------------------------
%% 角色查询服务
%%----------------------------------------------------
-module(role_query).
-behaviour(gen_server).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
-export([
        start_link/0
        ,sync_online_cache/1, clean_online_cache/1
        ,kick_all_role/0
    ]
).
-include("common.hrl").
-include("role.hrl").

-record(state, {
    }
).

%% ----------------------------------------------------
%% 外部接口
%% ----------------------------------------------------
%% @doc 启动
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

%% @doc 更新在线缓存
sync_online_cache(Role) ->
    {ok, OnlineCache} = role_convert:to(online_cache, Role),
    gen_server:cast(?MODULE, {sync_online_cache, OnlineCache}).

%% @doc 清除在线缓存
clean_online_cache(RoleId) ->
    gen_server:cast(?MODULE, {clean_online_cache, RoleId}).

%% @doc 踢所有在线角色下线
kick_all_role() ->
    gen_server:cast(?MODULE, kick_all_role).


%% ----------------------------------------------------
%% 内部处理
%% ----------------------------------------------------

init([]) ->
    ?INFO("[~w] 正在启动...", [?MODULE]),
    process_flag(trap_exit, true),
    ets:new(ets_role_online_cache, [set, named_table, public, {keypos, #role_online_cache.id}]),
    State = #state{},
    ?INFO("[~w] 启动完成", [?MODULE]),
    {ok, State}.

handle_call(_Request, _From, State) ->
    {noreply, State}.

handle_cast({sync_online_cache, OnlineCache}, State) ->
    ets:insert(ets_role_online_cache, OnlineCache),
    {noreply, State};
handle_cast({clean_online_cache, RoleId}, State) ->
    ets:delete(ets_role_online_cache, RoleId),
    {noreply, State};

handle_cast(kick_all_role, State) ->
    L = ets:tab2list(ets_role_online_cache),
    lists:foreach(fun(#role_online_cache{pid = Pid}) ->
                role:stop(Pid)
        end, L),
    {noreply, State};

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

%% ----------------------------------------------------
%% 私有函数
%% ----------------------------------------------------
