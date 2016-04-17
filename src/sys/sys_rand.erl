%%----------------------------------------------------
%% 随机数种子服务器
%%----------------------------------------------------
-module(sys_rand).
-behaviour(gen_server).
-export([
        start_link/0
        ,get_seed/0
    ]
).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
-include("common.hrl").
-record(state, {seed}).

%% ----------------------------------------------------
%% 对外接口
%% ----------------------------------------------------

%% @doc 启动服务器
-spec start_link() -> {ok, pid()} | ignore | {error, term()}.
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

%% @doc 取得一个随机数种子
-spec get_seed() -> {pos_integer(), pos_integer(), pos_integer()}.
get_seed() ->
    gen_server:call(?MODULE, get_seed).

%% ----------------------------------------------------
%% 内部调用处理
%% ----------------------------------------------------

init([]) ->
    {ok, #state{}}.

%% 返回一个随机数组合做为其它进程的随机数种子
handle_call(get_seed, _From, State) ->
    case State#state.seed of
        undefined -> random:seed(erlang:now());
        S -> random:seed(S)
    end,
    Seed = {random:uniform(99999), random:uniform(999999), random:uniform(999999)},
    {reply, Seed, State#state{seed = Seed}};

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
