%%----------------------------------------------------
%% 位置信息数据结构定义
%%----------------------------------------------------

%% 位置信息历史记录
-record(pos_history, {
        %% 地图ID
        map = 0             :: non_neg_integer()
        %% 平台标识
        ,platform = <<>>    :: bitstring()
        %% 区号
        ,zone_id = 0        :: non_neg_integer()
        %% 地图基础ID
        ,map_base_id = 0        :: non_neg_integer()
        %% x坐标
        ,x = 0              :: non_neg_integer()
        %% y坐标
        ,y = 0              :: non_neg_integer()
    }
).

%% 位置信息
-record(pos, {
        %% 当前所在地图id
        map_id = 10001         :: non_neg_integer()
        %% 最后所在的位置(进入特殊场景前一刻所在的位置)
        ,last               :: undefined | #pos_history{}
        %% 当前地图base_id
        ,map_base_id = 0    :: non_neg_integer()
        %% 当前地图PID
        ,map_pid = 0        :: non_neg_integer() | pid()
        %% 当前x坐标
        ,x = 0              :: non_neg_integer()
        %% 当前y坐标
        ,y = 0              :: non_neg_integer()
        %% 角色面对的方向
        ,dir = 0            :: 0..7
    }
).
