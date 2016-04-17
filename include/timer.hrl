%%----------------------------------------------------
%% 定时器数据结构
%%----------------------------------------------------

%% 全局定时器类型
-define(global_timer_type_everyday, 1).     %% 每天固定时间
-define(global_timer_type_fixed_time, 2).   %% 固定间隔时间


%% 角色定时器数据结构
-record(timer, {
        %% 定时器ID
        id                  :: term()
        %% 触发时间CD(毫秒)
        ,timeout = 1000     :: pos_integer()
       %% 回调模块
        ,m                  :: undefined | atom()
        %% 回调函数
        ,f                  :: function() | atom()
        %% 参数函数，在回调时系统会自动在前面添加Role参数
        ,a = []             :: [term()]
        %% 调用次数(-1表示无限次)
        ,times = 1          :: integer()

        %% 下次触发时间(由内部自动设置)
        ,next_tick = 0      :: non_neg_integer()
        %% 设定起始时间(由内部自动设定)
        ,set_time = 1       :: pos_integer()
    }
).

%% 角色中的timer相关数据
-record(rtimer, {
        %% 当前定时器引用
        ref                 :: undefined | reference()
        %% 已启动的定时器
        ,timers = []        :: [#timer{}]
    }
).

%% 全局定时器数据结构
-record(global_timer, {
        %% 标记
        label           :: atom() | string()
        %% 类型
        ,type = 0        :: non_neg_integer()
        %% 日期（年月日时分秒）
        ,datetime = {0, 0, 0, 0, 0, 0}     :: {non_neg_integer(), non_neg_integer(), non_neg_integer(), non_neg_integer(), non_neg_integer(), non_neg_integer()}
        %% 离下一次触发的时间（毫秒）
        ,timeout = 0    :: non_neg_integer()
        %% 定时器引用
        ,timer_ref      :: reference()
        %% 回调模块
        ,m              :: atom()
        %% 回调函数
        ,f              :: function() | atom()
        %% 参数
        ,a = []         :: [term()]
    }
).
