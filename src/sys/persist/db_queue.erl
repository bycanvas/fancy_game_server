%%----------------------------------------------------
%% 数据保存队列
%%----------------------------------------------------
-module(db_queue).

-behaviour(gen_server).

-export([
        start_link/1,
        init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3
    ]
).
-export([
        save_role/1
        ,db_queue_process_name/1
    ]
).

-include("common.hrl").
-include("protocol.hrl").
-include("role.hrl").
-include("persist.hrl").

-record(state, {
        id = 0
    }
).


%% -----------------------------------
%% 对外接口
%% -----------------------------------
%% 启动进程
-spec start_link(Id :: pos_integer()) -> {ok, Pid :: pid()} | {error, Reason :: atom()}.
start_link(Id) ->
    gen_server:start_link(?MODULE, [Id], []).

%% @doc 保存角色数据
save_role(Role = #role{id = {Rid, _, _}}) ->
    gen_server:cast(db_queue_process_name((Rid rem ?DB_QUEUE_NUM)+1), {save_role, Role}).

%% @doc 数据库写入队列进程名字
db_queue_process_name(Id) ->
    case env:get({db_queue_proc, Id}) of
        undefined ->
            Name = list_to_atom("db_queue_proc_" ++ integer_to_list(Id)),
            env:set({db_queue_proc, Id}, Name),
            Name;
        Name ->
            Name
    end.


%% ------------------------
%% 内部处理
%% ------------------------
init([Id]) ->
    ?INFO("[~w id=~w] 正在启动...", [?MODULE, Id]),
    process_flag(trap_exit, true),
    erlang:register(db_queue_process_name(Id), self()),
    State = #state{},
    ?INFO("[~w id=~w] 启动完成", [?MODULE, Id]),
    {ok, State}.

handle_call(_Request, _From, State) ->
    {noreply, State}.

handle_cast({save_role, Role}, State) ->
    case catch role_data:local_save(to_db, Role) of
        ok ->
            role_data:clean_cache(Role#role.id);
        {error, Err} ->
            ?ERR("角色数据[~ts]同步到数据库时发生异常，请勿清空role_data.dets: ~w", [Role#role.name, Err]);
        _Other ->
            ?ERR("角色数据[~ts]同步到数据库时发生异常，请勿清空role_data.dets: ~w", [Role#role.name, _Other])
    end,
    {noreply, State};

handle_cast(Msg, State) ->
    ?ERR("收到未知消息:~w", [Msg]),
    {noreply, State}.

handle_info(Info, State) ->
    ?ERR("收到未知消息:~w", [Info]),
    {noreply, State}.

terminate(_Reason, #state{id = _Id}) ->
    ?INFO("[~w id=~w] 正在关闭...", [?MODULE, _Id]),
    ?INFO("[id=~w]消息队列里还有:~w", [_Id, process_info(self(), message_queue_len)]),
    ?INFO("[~w id=~w] 关闭完成", [?MODULE, _Id]),
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% =======================================
%% 私有函数
%% =======================================
