%%----------------------------------------------------
%% 系统环境变量管理器
%% 注意: env.cfg中的值优先级最高，系统中原来保存的同名变量值将会被覆盖
%%----------------------------------------------------
-module(env).
-behaviour(gen_server).
-export([
        start_link/0
        ,start_link/1
        ,get/1
        ,get_default/2
        ,set/2
        ,update_counter/2
        ,save/2
        ,save_counter/2
        ,del/1
        ,dump/2
    ]
).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-record(state, {}).

-include("common.hrl").

%%----------------------------------------------------
%% 外部接口
%%----------------------------------------------------

%% @doc 取出指定的环境变量值
%% <div>注意: 如果不存预设置，则一律返回undefined</div>
%% <div>注意: 当环境变量服务器异常时取值也会返回undefined，需要在代码中小心应对</div>
-spec get(any()) -> undefined | any().
get(Key) ->
    case catch ets:lookup(env, Key) of
        [{_, Val}] -> Val;
        {'EXIT', {badarg, _}} ->
            ?ERR("系统环境变量服务器异常：未运行或已经崩溃"),
            undefined;
        _ ->
            undefined
    end.

%% @doc 取出指定的环境变量值
%% <div>注意: 如果不存预设置，则一律返回undefined</div>
%% <div>注意: 当环境变量服务器异常时取值也会返回undefined，需要在代码中小心应对</div>
-spec get_default(any(),any()) -> any().
get_default(Key, Default) ->
    case catch env:get(Key) of
        undefined ->
            set(Key, Default),
            Default;
        Val ->
            Val
    end.

%% @doc 设置一个环境变量值
%% <div>注意:进行并发操作get/set同一key时的事务性需要使用者来保证，或考虑使用update_counter/2</div>
%% <div>注意:设置的值并不会被保存，如果需要保存则使用save/2</div>
-spec set(any(), any()) -> any().
set(Key, Val) ->
    ets:insert(env, {Key, Val}),
    Val.

%% @doc 更新某个计数器
-spec update_counter(term(), integer()) -> integer().
update_counter(Key, Val) when is_integer(Val) ->
    ets:update_counter(env, Key, Val).

%% @doc 保存一个环境变量值
%% <div>注意:写入性能并不高(写入一次大约需要几十微秒)，所以不要用于需大量重复写入的情况</div>
-spec save(any(), any()) -> any().
save(Key, Val) ->
    ets:insert(env, {Key, Val}),
    dets:insert(env_file, {Key, Val}),
    Val.

%% @doc 更新并保存某个计数器
-spec save_counter(term(), integer()) -> integer().
save_counter(Key, Val) ->
    V = ets:update_counter(env, Key, Val),
    dets:insert(env_file, {Key, V}),
    V.

%% @doc 删除一个环境变量
-spec del(any()) -> ok | {error, term()}.
del(Key) ->
    ets:delete(env, Key),
    dets:delete(env_file, Key),
    ok.

%% @doc 导出指定变量的值到文件
-spec dump(any(), string()) -> ok | {error, term()}.
dump(Key, File) ->
    util:save(File, ?MODULE:get(Key)).

%% @doc 启动环境变量服务器，不加载配置文件
-spec start_link() -> ignore | {error, term()} | {ok, pid()}.
start_link() ->
    start_link("").

%% @doc 启动环境变量服务器(加载指定的配置文件)
-spec start_link(string()) -> ignore | {error, term()} | {ok, pid()}.
start_link(CfgFile) ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [CfgFile], []).

%%----------------------------------------------------
%% 内部处理
%%----------------------------------------------------

init([Cfg]) ->
    %% 环境变量存储器
    process_flag(trap_exit, true),
    ets:new(env, [set, named_table, public, {keypos, 1}]),
    dets:open_file(env_file, [{file, "./dets/env.dets"}, {keypos, 1}, {type, set}]),
    dets:to_ets(env_file, env),
    ZonePath = filename:absname(""),
    set(zone_path, ZonePath),
    State = #state{},
    case Cfg of
        "" -> {ok, State};
        _ ->
            case load_env_cfg(ZonePath ++ "/env.cfg") of
                {error, Reason} ->
                    ?ERR("载入配置[~ts]失败: ~w", [ZonePath ++ "/env.cfg", Reason]),
                    {stop, {can_not_load_env_file, Reason}};
                ok ->
                    io:format("~n~n+------------------------------------------------------------+~n"),
                    io:format("|                           loading...                       |~n"),
                    io:format("+------------------------------------------------------------+~n"),
                    ?INFO("!!!请核对服务器时间!!!"),
                    true = erlang:set_cookie(node(), env:get(cookie)),
                    ?INFO("已载入[~ts]的配置内容", [Cfg]),
                    erlang:send_after((date:unixtime(tomorrow) - date:unixtime()) * 1000, self(), loop),
                    {ok, State}
            end
    end.

handle_call(_Request, _From, State) ->
    {noreply, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(loop, State) ->
    erlang:send_after((date:unixtime(tomorrow) - date:unixtime() + 3000) * 1000, self(), loop),
    {noreply, State};

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    dets:close(env_file),
    ?DEBUG("env服务已关闭"),
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%----------------------------------------------------
%% 私有函数
%%----------------------------------------------------
%% 从指定的文件中加载变量设置 -> ok | {error, term()}.
load_env_cfg(File) ->
    case util:load(File) of
        {error, Reason} -> {error, Reason};
        {ok, undefined} -> ok;
        {ok, L} ->
            [set(K, V) || {K, V} <- L],
            ok
    end.
