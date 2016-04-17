%%----------------------------------------------------
%% 角色管理器
%%----------------------------------------------------
-module(role_mgr).
-behaviour(gen_server).
-export([
        start_link/0
        ,role_login/3
        ,logout/1
    ]
).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-include("common.hrl").
-include("role.hrl").
-include("pos.hrl").
-include("link.hrl").

-record(state, {
    }
).

%% ----------------------------------------------------
%% 对外接口
%% ----------------------------------------------------
%% @doc 启动
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

%% @doc 角色登录
-spec role_login(role_id(), bitstring(), #link{}) -> {ok, pid()} | {error, term()}.
role_login(RoleId, Account, Link) ->
    ?CALL(?MODULE, {login, RoleId, Account, Link}, infinity).

%% @doc 角色离线
logout(#role{id = RoleId}) ->
    gen_server:cast(?MODULE, {logout, RoleId}).


%% ----------------------------------------------------
%% 内部处理
%% ----------------------------------------------------
init([]) ->
    ?INFO("[~w] 正在启动...", [?MODULE]),
    process_flag(trap_exit, true),
    ok = do_init(),
    ?INFO("[~w] 启动完成", [?MODULE]),
    {ok, #state{}}.

handle_call({login, RoleId, Account, Link}, _From, State) ->
    case ets:lookup(ets_role_login, RoleId) of
        [] -> %% 角色进程不存在，创建
            do_create(RoleId, Account, Link, State);
        [#role_login_info{pid = RolePid}] -> %% 角色进程已存在，顶号
            do_switch(RolePid, RoleId, Account, Link, State)
    end;

handle_call(_Request, _From, State) ->
    {noreply, State}.

handle_cast({logout, RoleId}, State) ->
    ets:delete(ets_role_login, RoleId),
    {noreply, State};

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State = #state{}) ->
    ?INFO("玩家数据服务进程关闭: ~w", [_Reason]),
    role_query:kick_all_role(),
    dets:close(role_data),
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% ----------------------------------------------------
%% 私有函数
%% ----------------------------------------------------
%% 启动重载 -> ok
do_init() ->
    ets:new(ets_role_login, [set, named_table, protected, {keypos, #role_login_info.id}]),
    ok.

do_create(RoleId, Account, Link, State) ->
    case role:create(RoleId, Account, Link) of
        {ok, RolePid} ->
            ets:insert(ets_role_login, #role_login_info{id = RoleId, account = Account, pid = RolePid}),
            {reply, {ok, RolePid}, State};
        {error, Err} ->
            {reply, {error, Err}, State}
    end.

do_switch(RolePid, RoleId, Account, Link, State) ->
    case role:fix_link(RolePid, RoleId, Account, Link) of
        ok ->
            {reply, {ok, RolePid}, State};
        {false, Msg} ->
            {reply, {error, Msg}, State};
        _Err ->
            ?ERR("[~w]顶号失败:~w", [RoleId, _Err]),
            {reply, {error, ?T("登录失败，请稍后重试")}, State}
    end.
