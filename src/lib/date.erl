%%----------------------------------------------------
%% 日期函数，作为calendar模块的补充，calendar模块已有的功能不再提供
%%----------------------------------------------------
-module(date).
-export(
    [
        unixtime/0
        ,unixtime/1
        ,is_valid/2
        ,is_same_day/2
        ,is_same_week/2
        ,now_diff/2
        ,next_diff/3
        ,next_diff/1
        ,diff/2
        ,diff/3
        ,day_diff/2
        ,datetime_to_seconds/1
        ,seconds_to_datetime/1
        ,datetime_to_timestamp/1
        ,is_today/1
        ,day_of_the_week/1
        ,day_of_the_week/3
        ,format_datetime/1
        ,last_day_of_the_month/2
        ,get_date/1
        ,get_unixtime_delay/1
        ,seconds_to_date/2
    ]
).

-define(DAYS_PER_YEAR, 365).
-define(DAYS_PER_LEAP_YEAR, 366).

%% @doc 取得当前的unix时间戳
-spec unixtime() -> pos_integer().
unixtime() ->
    %% {M, S, _} = erlang:now(),
    {M, S, _} = os:timestamp(),
    M * 1000000 + S.


%% @doc 获取N天前的日期，返回 Y-M-D
-spec get_date(N::integer()) -> L::list.
get_date(N) when is_integer(N) ->
    Delay = (N) * 86400,
    UnixT = date:unixtime() - Delay,
    {{Y, M, D}, {_H, _I, _S}}= seconds_to_datetime(UnixT), 
    lists:concat([Y, "-", M, "-", D]).

%% 返回指定的距离现在第N天的 00:00:00 至 23:59:59 的时间戳范围
-spec get_unixtime_delay(N::integer()) -> T::tuple.
get_unixtime_delay(N) ->
    ExDate = get_date(N),
    St_str = lists:concat([ExDate, " 00:00:00"]), 
    Et_str = lists:concat([ExDate, " 23:59:59"]),
    St = date:format_datetime(St_str),
    Et = date:format_datetime(Et_str),
    {St, Et}.

%% 返回该时间戳对应的日期，格式 -> "2015-3-26"
seconds_to_date(TimeStamp, false) ->
    {{Y, M, D}, {_H, _I, _S}}= date:seconds_to_datetime(TimeStamp), 
    lists:concat([Y, "-", M, "-", D]);

%% 返回该时间戳对应的日期，格式 -> "2015-3-26 15:09:09"
seconds_to_date(TimeStamp, true) ->
    {{Y, M, D}, {H, I, S}}= date:seconds_to_datetime(TimeStamp), 
    lists:concat([Y, "-", M, "-", D, " ", H, ":", I, ":", S]).

%% @doc 返回相应类型的unix时间戳
%% <ul>
%% <li>ms: 取得当前的unix时间戳，精确到毫秒</li>
%% <li>today: 获取当天0时0分0秒的时间戳</li>
%% <li>{today, Ts}: 根据给出的时间戳，获取与该时间戳同一天的零时。当时间为0时，返回值有可能是负值，因为这里有时区偏移值(例如北京时间就可能是-28800)</li>
%% <li>{tomorrow, Ts}: 根据给出的时间戳，获取该时间戳第二天的零时</li>
%% <li>{next_time, Ts}::根据给出的时间间隔（距离0点的时间, 如3600），取出下一个该时间戳</li>
%% </ul>
-spec unixtime(X) -> pos_integer() when
    X :: ms | today | {today, pos_integer()} | {next_day, pos_integer()}.
unixtime(ms) ->
    %% {S1, S2, S3} = erlang:now(),
    {S1, S2, S3} = os:timestamp(),
    trunc(S1 * 1000000000 + S2 * 1000 + S3 / 1000);
unixtime(today) ->
    {M, S, MS} = os:timestamp(),
    {_, Time} = calendar:now_to_local_time({M, S, MS}),
    M * 1000000 + S - calendar:time_to_seconds(Time);
unixtime(tomorrow) ->
    unixtime(today) + 86400;
unixtime({today, Ts}) ->
    Base = unixtime(today),
    case Ts > Base of
        false -> Base - util:ceil((Base - Ts) / 86400) * 86400;
        true -> (Ts - Base) div 86400 * 86400 + Base
    end;
unixtime({tomorrow, Ts}) ->
    unixtime({today, Ts}) + 86400;
unixtime({next_time, DayTs}) ->
    Now = unixtime(),
    NextT = next_diff(DayTs),
    Now + NextT.

%% @doc 指定的时间戳与当前时间相比
%% @see diff/3
-spec diff(atom(), pos_integer()) -> pos_integer().
diff(T, Ts) ->
    diff(T, Ts, unixtime(ms)).

%% @doc 比较两个时间戳
%% <div>注意: 该函数只返回整数，比如3.5天会变成3天，也就是说比较的意思是相距3天或以上</div>
%% <ul>
%% <li>second: 比较两个时间相差的秒数<div>
%% <li>minute: 比较两个时间相差的分钟数<div>
%% <li>hour: 比较两个时间相差的小时数<div>
%% <li>day: 比较两个时间相差的天数</div>
%% </ul>
-spec diff(atom(), pos_integer(), pos_integer()) -> pos_integer().
diff(second, Ts1, Ts2) ->
    Ts1 - Ts2;
diff(minute, Ts1, Ts2) ->
    trunc((Ts1 - Ts2) / 60);
diff(hour, Ts1, Ts2) ->
    trunc((Ts1 - Ts2) / 3600);
diff(day, Ts1, Ts2) ->
    trunc((Ts1 - Ts2) / 86400).

%% @doc 两个unixtime相差的天数,相邻2天返回1
%% return int() 相差的天数
day_diff(FromTime, ToTime) when ToTime > FromTime ->
    FromDate = unixtime({today, FromTime}),
    ToDate = unixtime({today, ToTime}),
    case (ToDate - FromDate) / 86400 of
        Diff when Diff < 0 -> 0;
        Diff -> round(Diff)
    end;
day_diff(FromTime, ToTime) when ToTime =:= FromTime -> 0;
day_diff(FromTime, ToTime) -> day_diff(ToTime, FromTime).

%% @doc 取得当前距离指定时间下次到达时相差的秒数
-spec next_diff(H, M, S) -> Seconds when
    H :: 0..23,
    M :: 0..59,
    S :: 0..59,
    Seconds :: pos_integer().
next_diff(H, M, S) ->
    Sec = H * 3600 + M * 60 + S,
    next_diff(Sec).

-spec next_diff(0..86400 | [0..86400]) -> Seconds::pos_integer().
next_diff(L = [_ | _]) ->
    lists:min([next_diff(Sec) || Sec <- L]);
next_diff(Sec) ->
    Now = unixtime(),
    Zero = unixtime({today, Now}),
    Base = Zero + Sec, %% 取当天距离X的时间为指定时间
    case Base > Now of 
        true -> Base - Now; %% 当前时间比指定时间小 直接返回差距
        false -> Base + 86400 - Now %% 当前时间比指定时间大 加上一天时间后求差
    end.

%% @doc 当前距离N天后某时刻的相差秒数
-spec now_diff(N, {H, I, S}) -> integer() when
    N :: today | tomorrow | non_neg_integer(),
    H :: 0..23,
    I :: 0..59,
    S :: 0..59.
now_diff(today, {H, I, S})  ->
    now_diff(0, {H, I, S});
now_diff(tomorrow, {H, I, S}) ->
    now_diff(1, {H, I, S});
now_diff(N, {H, I, S}) when is_integer(N), N >= 0 ->
    {_, Time} = calendar:local_time(),
    N * 86400 + calendar:time_to_seconds({H, I, S}) - calendar:time_to_seconds(Time).

%% @doc 检测是否为有效的日期或时间
-spec is_valid(T, Datetime) -> boolean() when
    T :: date | time | datetime,
    Datetime :: {Y, M, D} | {H, I, S} | {{Y, M, D}, {H, I, S}},
    Y :: pos_integer(),
    M :: pos_integer(),
    D :: pos_integer(),
    H :: non_neg_integer(),
    I :: non_neg_integer(),
    S :: non_neg_integer().
is_valid(date, Date) ->
    calendar:valid_date(Date);
is_valid(time, {H, I, S}) ->
    H >= 0 andalso H < 24 andalso I >= 0 andalso I < 60 andalso S >= 0 andalso S < 60;
is_valid(datetime, {Date, Time}) ->
    calendar:valid_date(Date) andalso is_valid(time, Time);
is_valid(_, _) ->
    false.

%% @doc 比较两个unixtime是否同一天
-spec is_same_day(Ts1, Ts2) -> boolean() when
    Ts1 :: non_neg_integer(),
    Ts2 :: non_neg_integer().
is_same_day(0, Ts2) when Ts2 > 0 -> false;
is_same_day(Ts1, 0) when Ts1 > 0 -> false;
is_same_day(Ts1, Ts2) ->
    unixtime({today, Ts1}) =:= unixtime({today, Ts2}).

%% @doc 比较两个unixtime是否同一周
-spec is_same_week(Ts1, Ts2) -> boolean() when
    Ts1 :: non_neg_integer(),
    Ts2 :: non_neg_integer().
is_same_week(0, Ts2) when Ts2 > 0 -> false;
is_same_week(Ts1, 0) when Ts1 > 0 -> false;
is_same_week(Ts, Ts) -> true;
is_same_week(Ts1, Ts2) ->
    Dw = day_of_the_week(Ts1),
    Zero = unixtime({today, Ts1}),
    Beg = Zero - 86400 * (Dw-1),
    End = Zero + 86400 * (8-Dw),
    Ts2 >= Beg andalso Ts2 < End.

%% @doc 判断一个时间戳是否为今天
-spec is_today(pos_integer()) -> boolean().
is_today(Unixtime) ->
    unixtime(today) =:= unixtime({today, Unixtime}).

%% @doc 将日期转换unix时间戳
-spec datetime_to_seconds(DateTime) -> false | Seconds when
    DateTime :: {{Y, M, D}, {H, M, S}},
    Y :: pos_integer(),
    M :: 1..12,
    D :: 1..31,
    H :: 0..23,
    M :: 0..59,
    S :: 0..59,
    Seconds :: pos_integer().
datetime_to_seconds({Year, Month, Day, Hour, Minute, Second}) ->
    datetime_to_seconds({{Year, Month, Day}, {Hour, Minute, Second}});
datetime_to_seconds(DateTime) ->
    case calendar:local_time_to_universal_time_dst(DateTime) of
        [] -> false;
        [_, Udate] -> 
            calendar:datetime_to_gregorian_seconds(Udate) - 719528 * 24 * 3600;
        [Udate] ->
            calendar:datetime_to_gregorian_seconds(Udate) - 719528 * 24 * 3600
    end.

%% @doc 将Unixtime转换成当地时间
-spec seconds_to_datetime(Unixtime) -> {{Y :: non_neg_integer(), M :: non_neg_integer(), D:: non_neg_integer()}, {HH :: non_neg_integer(), MM :: non_neg_integer, SS :: non_neg_integer()}} when
    Unixtime :: non_neg_integer().
seconds_to_datetime(Unixtime) ->
    T1 = Unixtime div 1000000,
    T2 = Unixtime - T1 * 1000000,
    T = calendar:now_to_datetime({T1, T2, 0}),
    calendar:universal_time_to_local_time(T).

%seconds_to_datetime(Unixtime) ->
%    Local = erlang:universaltime_to_localtime({{1970, 1, 1}, {0,0,0}}),
%    LocalStamp = calendar:datetime_to_gregorian_seconds(Local),
%    TimeStamp = Unixtime + LocalStamp,
%    calendar:gregorian_seconds_to_datetime(TimeStamp).

%% 日期转时间戳 DateTime :: {{2013,11,13}, {18,0,0}}
-spec datetime_to_timestamp(DateTime :: {{non_neg_integer(), non_neg_integer(), non_neg_integer()},{non_neg_integer(), non_neg_integer(), non_neg_integer()}}) -> non_neg_integer().
datetime_to_timestamp(DateTime) ->
    calendar:datetime_to_gregorian_seconds(DateTime) -
    calendar:datetime_to_gregorian_seconds({{1970,1,1}, {8,0,0}}).

%% @doc 把Unixtime转换成星期几
-spec day_of_the_week(Unixtime :: non_neg_integer()) -> 1..7.
day_of_the_week(Unixtime) ->
    {{Y, M, D}, _} = seconds_to_datetime(Unixtime),
    day_of_the_week(Y, M ,D).

%% @doc 把日期转换成星期几
-spec day_of_the_week(Year :: non_neg_integer(), Month :: non_neg_integer(), Day :: non_neg_integer()) -> 1..7.
day_of_the_week(Year, Month, Day) ->
    (date_to_gregorian_days(Year, Month, Day) + 5) rem 7 + 1.

%% @doc 日期字符串格式化成unixtime
%% DateStr = "2014-06-01 00:00:00"
-spec format_datetime(DateStr :: list()) -> non_neg_integer().
format_datetime(DateStr) when is_list(DateStr) ->
    [A, B] = string:tokens(DateStr, " "),
    [SYear, SMonth, SDay] = string:tokens(A, "-"),
    [SHour, SMin, SSec] = string:tokens(B, ":"),
    Year = list_to_integer(SYear),
    Month = list_to_integer(SMonth),
    Day = list_to_integer(SDay),
    Hour = list_to_integer(SHour),
    Min = list_to_integer(SMin),
    Sec = list_to_integer(SSec),
    date:datetime_to_seconds({Year, Month, Day, Hour, Min, Sec});
format_datetime(DateStr) -> DateStr.


%% ========================================
%% 内部方法
%% ========================================
%% 计算离0000年1月1日到现在有多少天
date_to_gregorian_days(Year, Month, Day) when is_integer(Day), Day > 0 ->
    Last = last_day_of_the_month(Year, Month),
    if
        Day =< Last ->
            dy(Year) + dm(Month) + df(Year, Month) + Day -1
    end.

%% 计算指定年月有多少天 -> 28..31
last_day_of_the_month(Year, Month) when is_integer(Year), Year >= 0 ->
    do_last_day_of_the_month(Year, Month).
do_last_day_of_the_month(_, 4) -> 30;
do_last_day_of_the_month(_, 6) -> 30;
do_last_day_of_the_month(_, 9) -> 30;
do_last_day_of_the_month(_, 11) -> 30;
do_last_day_of_the_month(Y, 2) ->
    case is_leap_year(Y) of
        true -> 29;
        false -> 28
    end;
do_last_day_of_the_month(_, M) when is_integer(M), M>0, M<13 -> 31.

%% 是否闰年 -> true | false
is_leap_year(Year) when Year rem 4 =:= 0, Year rem 100 > 0 ->
    true;
is_leap_year(Year) when Year rem 400 =:= 0 ->
    true;
is_leap_year(_) -> false.

%% Days in previous years
dy(Year) when Year =< 0 -> 0;
dy(Year) ->
    X = Year - 1,
    (X div 4) - (X div 100) + (X div 400) + X * ?DAYS_PER_YEAR + ?DAYS_PER_LEAP_YEAR.

%% Returns the total number of days in all months preceeding month, for an oridinary year
dm(1) -> 0;
dm(2) -> 31;
dm(3) -> 59;
dm(4) -> 90;
dm(5) -> 120;
dm(6) -> 151;
dm(7) -> 181;
dm(8) -> 212;
dm(9) -> 243;
dm(10) -> 273;
dm(11) -> 304;
dm(12) -> 334.

%% Accounts for an extra day in February if Year is a leap year, and if Month > 2
df(_, Month) when Month < 3 -> 0;
df(Year, _) ->
    case is_leap_year(Year) of
        true -> 1;
        false -> 0
    end.
