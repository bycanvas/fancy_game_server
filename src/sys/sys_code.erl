%%----------------------------------------------------
%% 代码管理器
%%----------------------------------------------------
-module(sys_code).
-behaviour(gen_server).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
-export([
        start_link/0
        ,beam_file/0
        ,beam_hash/0
        ,beam_hash/1
        ,up/0
        ,up/1
        ,up/2
    ]
).

-include("common.hrl").
-record(state, {}).

%% ----------------------------------------------------
%% 对外接口
%% ----------------------------------------------------

%% @doc 启动
-spec start_link() -> {ok, pid()} | ignore | {error, term()}.
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

%% @doc 获取ebin目录下的beam文件列表
-spec beam_file() -> list().
beam_file() ->
    case file:list_dir(env:get(code_path) ++ "/ebin") of
        {ok, FList} ->
            do_beam_file(FList, []);
        {error, Why} ->
            ?ERR("获取beam文件时发生异常: ~w", [Why]),
            []
    end.

%% @doc 获取所有beam文件的hash值
-spec beam_hash() -> list().
beam_hash() ->
    do_beam_hash(beam_file(), []).

%% @doc 获取指定beam文件的hash值
-spec beam_hash(string()) -> list().
beam_hash(M) ->
    do_beam_hash([M], []).

%% @doc 重新检查并加载所有的模块
-spec up() -> list().
up() ->
    O = ets:tab2list(sys_code),
    N = beam_hash(),
    do_up(N, O, [], fun code:soft_purge/1).

%% @doc 重新检查并加载所有的模块，并强制清除运行中的旧代码
%% <ul>
%% <li>force 处理所有模块</li>
%% <li>[ModName] 处理所有指定的模块</li>
%% </ul>
%% code:load_file :将新的代码加载到内存中,内存中如果已经存在此模块代码那么标识为老的(如果有进程正在使用(一般是递归或者循环)的话那么还是可以用老版本),如果内存中已经存在此模块的老版本代码,则此方法会返回not_purded错误,执行执行 purde后才能将新的文件加载到内存,无论内存中存在几个版本,新的进程一定会使用新版本代码.
-spec up(Option) -> list() when
    Option :: force | [ModName],
    ModName :: atom().
up(force) ->
    N = beam_hash(),
    O = ets:tab2list(sys_code),
    do_up(N, O, [], fun code:purge/1);
up(ModList) ->
    O = ets:tab2list(sys_code),
    N = do_beam_hash(ModList, []),
    do_up(N, O, [], fun code:soft_purge/1).

%% @doc 重新检查并加载指定的模块，并强制清除运行中的旧代码
-spec up([ModName], force) -> list() when
    ModName :: atom().
up(ModList, force) ->
    O = ets:tab2list(sys_code),
    N = do_beam_hash(ModList, []),
    do_up(N, O, [], fun code:purge/1).

%% ----------------------------------------------------
%% 内部实现
%% ----------------------------------------------------

init([]) ->
    ?INFO("[~w] 正在启动...", [?MODULE]),
    ets:new(sys_code, [named_table, public, set]),
    [ets:insert(sys_code, X) || X <- beam_hash()],
    State = #state{},
    ?INFO("[~w] 启动完成", [?MODULE]),
    {ok, State}.

handle_call(_Request, _From, State) ->
    {noreply, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% ----------------------------------------------------
%% 私有函数
%% ----------------------------------------------------

%% 返回所有的beam文件
do_beam_file([], List) -> List;
do_beam_file([F | T], List) ->
    L = case filename:extension(F) =:= ".beam" of
        true ->
            M = filename:basename(filename:rootname(F)),
            [M | List];
        _ -> List
    end,
    do_beam_file(T, L).

%% 返回beam hash
do_beam_hash([], List) -> List;
do_beam_hash([N | T], List) ->
    L = case beam_lib:md5(env:get(code_path) ++ "/ebin/" ++ N) of
        {ok, {M, Md5}} ->
            [{M, util:md5(Md5)} | List];
        Err ->
            ?ERR("无法获取文件[~ts]的hash值:~w", [N, Err]),
            List
    end,
    do_beam_hash(T, L).

%% 执行更新
do_up([], _O, Rtn, _Fun) -> Rtn;
do_up([{Mod, NewHash} | N], O, Rtn, Fun) ->
    NewRtn = case lists:keyfind(Mod, 1, O) of
        false ->
            [load_beam(Mod, NewHash, Fun) | Rtn];
        {_, OldHash} ->
            case OldHash =:= NewHash of
                true -> Rtn;
                false -> [load_beam(Mod, NewHash, Fun) | Rtn]
            end
    end,
    do_up(N, O, NewRtn, Fun).

%% 加载beam文件(热更新)
load_beam(Mod, Hash, PurgeFun) ->
    PurgeFun(Mod),
    case code:load_file(Mod) of
        {module, _} ->
            ets:insert(sys_code, {Mod, Hash}),
            {Mod, ok};
        {error, Why} ->
            {Mod, {error, Why}}
    end.
