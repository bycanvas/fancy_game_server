%%----------------------------------------------------
%% 玩家数据获取/存储服务接口
%%----------------------------------------------------
-module(role_data).
-behaviour(gen_server).
-export([
        start_link/0
        ,role_init/1
        ,role_save/1
        ,create/1
        ,fetch/2
        ,save/1
    ]
).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-include("common.hrl").
-include("role.hrl").
-include("pos.hrl").

-record(state, {
        next_id = 0
    }
).

%% ----------------------------------------------------
%% 对外接口
%% ----------------------------------------------------
%% @doc 启动
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

%% @doc 初始化玩家数据结构
-spec role_init(#role{}) -> #role{}.
role_init(Role) -> Role.

%% @doc 玩家数据持久化存储前处理转换数据
%% <div>保存数据库前需要清除pid、port、ref、fun等信息</div>
-spec role_save(#role{}) -> #role{}.
role_save(Role = #role{
        pos = Pos
    }) ->
    Role#role{
        pid = 0
        ,link = undefined       %% 重置连接器
        ,pos = case is_record(Pos, pos) of
            true -> Pos#pos{map_pid = undefined};
            false -> Pos
        end
    }.

%% @doc 创建一个新角色
-spec create(#role{}) -> {ok, #role{}} | {error, term()}.
create(Role) ->
    ?CALL(?MODULE, {create, Role}, infinity).

%% @doc 获取指定的角色数据(支持离线、自动版本转换、跨服查询)
-spec fetch(by_id | by_acc, role_id() | role_acc()) -> {ok, #role{}} | false | {error, error_code()}.
fetch(by_id, RoleId) ->
    ?CALL(?MODULE, {fetch_by_id, RoleId}, infinity);
fetch(by_acc, Acc) ->
    ?CALL(?MODULE, {fetch_by_acc, Acc}, infinity).

%% @doc 保存玩家数据到DETS缓存
%% <div>缓存时保留PID等信息，写入数据库前需要重置</div>
-spec save(Role::#role{}) -> ok | {error, error_code()}.
save(Role) ->
    R = role_save(Role),
    dets:insert(role_data, R).


%% ----------------------------------------------------
%% 内部处理
%% ----------------------------------------------------
init([]) ->
    ?INFO("[~w] 正在启动...", [?MODULE]),
    process_flag(trap_exit, true),
    dets:open_file(role_data, [{type, set}, {keypos, #role.id}, {file, "./dets/role_data.dets"}]),
    ets:new(ets_role_acc, [set, named_table, protected, {keypos, 1}]), %% {{Account, Platform, ZoneId}, {Rid, Platform, ZoneId}}
    {ok, NextId} = do_init(),
    ?INFO("[~w] 启动完成", [?MODULE]),
    {ok, #state{next_id = NextId}}.

%% 创建玩家数据
handle_call({create, R = #role{id = {_, Platform, ZoneId}, account = Account}}, _From, State = #state{next_id = NextId}) ->
    case env:get(platform) =:= Platform andalso env:get(zone_id) =:= ZoneId of
        false ->
            {reply, {error, zone_id_error}, State};
        true ->
            Acc = {Account, Platform, ZoneId},
            case ets:lookup(ets_role_acc, Acc) of
                [] ->
                    RoleId = {NextId, Platform, ZoneId},
                    Role = role_init(R#role{id = RoleId}),
                    case save(Role) of
                        {error, Reason} ->
                            ?ERR("创建玩家数据时保存失败: ~w", [Reason]),
                            {reply, {error, Reason}, State};
                        ok ->
                            ets:insert(ets_role_acc, {Acc, RoleId}),
                            {reply, {ok, Role}, State#state{next_id = NextId+1}}
                    end;
                [_|_] ->
                    ?ERR("玩家账号[~ts, ~ts, ~w]创建数据失败: 同个分区重复创建数据", [Account, Platform, ZoneId]),
                    {reply, {error, exist}, State}
            end
    end;

%% 获取指定的角色数据(自动进行版本转换)
handle_call({fetch_by_acc, Acc}, _From, State) ->
    case ets:lookup(ets_role_acc, Acc) of
        [] ->
            {reply, false, State};
        [{Acc, RoleId}] ->
            case do_fetch(RoleId) of
                {ok, Role} ->
                    {reply, {ok, Role}, State};
                false ->
                    {reply, false, State};
                {error, Err} ->
                    {reply, {error, Err}, State}
            end
    end;
handle_call({fetch_by_id, RoleId}, _From, State) ->
    case do_fetch(RoleId) of
        {ok, Role} ->
            {reply, {ok, Role}, State};
        false ->
            {reply, false, State};
        {error, Err} ->
            {reply, {error, Err}, State}
    end;

handle_call(_Request, _From, State) ->
    {noreply, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State = #state{}) ->
    ?INFO("玩家数据服务进程关闭: ~w", [_Reason]),
    dets:close(role_data),
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% ----------------------------------------------------
%% 私有函数
%% ----------------------------------------------------
%% 启动重载 -> {ok, NextId} | error
do_init() ->
    put(max_role_id, 0),
    F = fun(R) ->
            %% 必须校验版本
            case role_ver:do(R) of
                {ok, Role = #role{id = RoleId = {Rid, Platform, ZoneId}, account = Account}} ->
                    dets:insert(role_data, Role),
                    ets:insert(ets_role_acc, {{Account, Platform, ZoneId}, RoleId}),
                    put(max_role_id, erlang:max(Rid, get(max_role_id))),
                    continue;
                {error, _E} ->
                    ?ERR("玩家数据转换失败ERR: ~w ROLE: ~w", [_E, R]),
                    {done, R} %% 退出遍历
            end
    end,
    case dets:traverse(role_data, F) of
        [] -> {ok, get(max_role_id) + 1};
        _Err ->
            ?ERR("玩家数据重载异常[ERR]: ~w", [_Err]),
            error
    end.

%% 从dets中获取角色数据 -> {ok, #role{}} | false | {error, term()}
do_fetch(RoleId) ->
    case dets:lookup(role_data, RoleId) of
        [Role] ->
            {ok, A} = role_ver:do(Role),
            {ok, A};
        [] ->
            false;
        {error, Err} ->
            ?ERR("获取玩家[~w]数据出错:~w", [RoleId, Err]),
            {error, Err}
    end.
