%%----------------------------------------------------
%% 全局定时器管理
%% @author yankai
%% @end
%%----------------------------------------------------
-module(global_timer).
-behaviour(gen_server).
-export([
        start_link/0
    ]
).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-include("common.hrl").
-include("role.hrl").
-include("timer.hrl").

-record(state, {
    }
).

%% ----------------------------------------------------
%% 外部接口
%% ----------------------------------------------------
%% @doc 启动
-spec start_link() -> {ok, pid()} | ignore | {error, term()}.
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).


%% ----------------------------------------------------
%% 内部处理
%% ----------------------------------------------------

init([]) ->
    ?INFO("[~w] 正在启动...", [?MODULE]),
    process_flag(trap_exit, true),
    State = #state{},
    init_timers(),
    ?INFO("[~w] 启动完成", [?MODULE]),
    {ok, State}.

handle_call(_Request, _From, State) ->
    {noreply, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info({tick, Timer = #global_timer{label = Label, m = M, f = F, a = A}}, State) ->
    case catch erlang:apply(M, F, A) of
        ok -> ok;
        _Err -> ?ERR("执行全局定时器[~w]的任务失败:~w", [Label, _Err])
    end,
    case calc_idle_time(Timer) of
        undefined ->
            ?ERR("全局定时器[~w]下一次无法执行，计算出来的时间错误", [Label]);
        IdleTime ->
            erlang:send_after(IdleTime, self(), {tick, Timer})
    end,
    {noreply, State};

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
%% 初始化全局定时器
%% 注意：设置了定时器的功能，本身也要有一定的保底机制，因为定时器只能对
init_timers() ->
    Timers = case env:get(node_type) of
        zone ->
            [
                #global_timer{label = arena_career_send_reward, type = ?global_timer_type_everyday, datetime = {0, 0, 0, 21, 0, 0}, m = arena_career, f = send_daily_reward}
                ,#global_timer{label = arena_career_reset_reward, type = ?global_timer_type_everyday, datetime = {0, 0, 0, 5, 0, 0}, m = arena_career, f = reset_daily_reward}
                ,#global_timer{label = log_guild_list, type = ?global_timer_type_everyday, datetime = {0, 0, 0, 4, 35, 0}, m = guild, f = log_guild_list}
                ,#global_timer{label = reset_fb_reply, type = ?global_timer_type_everyday, datetime = {0, 0, 0, 5, 30, 0}, m = chat_fb, f = reset_fb_reply}
                ,#global_timer{label = guild_auto_impeach, type = ?global_timer_type_everyday, datetime = {0, 0, 0, 5, 10, 0}, m = guild_common, f = auto_impeach}
                ,#global_timer{label = reset_wedding_time_list, type = ?global_timer_type_everyday, datetime = {0, 0, 0, 0, 0, 0}, m = wedding_mgr, f = auto_reset_wedding_time_list}
                ,#global_timer{label = reset_world_compete, type = ?global_timer_type_everyday, datetime = {0, 0, 0, 5, 0, 0}, m = world_compete_mgr, f = reset_all_signup_num}
                ,#global_timer{label = world_compete_send_reward, type = ?global_timer_type_everyday, datetime = {0, 0, 0, 0, 2, 0}, m = world_compete_mgr, f = send_daily_reward}
                ,#global_timer{label = star_officer, type = ?global_timer_type_everyday, datetime = {0, 0, 0, 5, 0, 0}, m = star_officer, f = create_npc}
                ,#global_timer{label = star_officer_effect, type = ?global_timer_type_everyday, datetime = {0, 0, 0, 5, 0, 0}, m = star_officer, f = add_effect}
            ];
        center ->
            [
                %% ...
            ];
        _ ->
            []
    end,
    ?INFO("启动的全局定时器个数:~w", [length(Timers)]),
    init_timers(Timers).
init_timers([]) -> ok;
init_timers([Timer|T]) ->
    case calc_idle_time(Timer) of
        undefined ->
            init_timers(T);
        IdleTime ->
            erlang:send_after(IdleTime, self(), {tick, Timer}),
            init_timers(T)
    end.

%% 计算下一次触发的时间 -> non_neg_integer() | undefined
calc_idle_time(#global_timer{type = ?global_timer_type_everyday, datetime = {_, _, _, Hour, Min, Sec}}) ->
    Time = Hour * 3600 + Min * 60 + Sec,
    get_idle_time(Time);
calc_idle_time(#global_timer{type = ?global_timer_type_fixed_time, datetime = {_, _, _, Hour, Min, Sec}}) ->
    Time = Hour * 3600 + Min * 60 + Sec,
    Time * 1000;
calc_idle_time(#global_timer{type = _Type}) ->
    ?ERR("类型[~w]的全局定时器未支持", [_Type]),
    undefined.

%% 获取等待时间 -> 毫秒
%% Time :: 每天的生效时间（单位：秒）
get_idle_time(Time) ->
    Now = date:unixtime(),
    Today = date:unixtime(today),
    IdleTime = case (Today + Time) > Now of
        true -> (Today + Time - Now);
        false -> date:unixtime({tomorrow, Now}) - Now + Time
    end,
    IdleTime * 1000.
