%%----------------------------------------------------
%% 地图相关数据结构定义
%% 
%% * 地图与地图之间通过连接点相连，由地图编辑器中设置
%% * 连接点信息:{目标地图ID, X坐标, Y坐标, [条件列表]}
%% * 连接点可以被副本管理器等控制
%% * 角色进入地图有两种方式:1:访问连接点, 2:使用传送道具
%%----------------------------------------------------

%% 移动坐标格子大小定义
-define(MOVE_GRID_SIZE, 32).

%% 限制小格子范围在有效范围内
-define(DX(X, W), if X =< 0 -> 0; X > W -> W; true -> X end).
-define(DY(Y, H), if Y =< 0 -> 0; Y > H -> H; true -> Y end).

%% 九宫格广播半径
-define(CAST_RADIUS_X, 320).    %% 50 * 10
-define(CAST_RADIUS_Y, 192).    %% 50 * 6

%% 像素坐标转成九宫格格子
-define(GX(X), util:ceil(X / ?CAST_RADIUS_X)).
-define(GY(Y), util:ceil(Y / ?CAST_RADIUS_Y)).

%% 像素大小转成九宫格大小
-define(GW(W), util:ceil(W / ?CAST_RADIUS_X)).
-define(GH(H), util:ceil(H / ?CAST_RADIUS_Y)).

%% 默认进入的地图
-define(DEFAULT_MAP_ID, 10000).

%% 可视范围角色列表变化类型
-define(MAP_ROLE_LIST_CHANGE_TYPE_ALL, 0).      %% 全部刷新
-define(MAP_ROLE_LIST_CHANGE_TYPE_ADD, 1).      %% 增加
-define(MAP_ROLE_LIST_CHANGE_TYPE_DEL, 2).      %% 移除

%% 角色地图移动类型
-define(MAP_ROLE_MOVE_TYPE_MOVE, 0).            %% 移动
-define(MAP_ROLE_MOVE_TYPE_ENTER_GRID, 1).      %% 进入格子
-define(MAP_ROLE_MOVE_TYPE_LEAVE_GRID, 2).      %% 离开格子
-define(MAP_ROLE_MOVE_TYPE_ENTER_MAP, 3).       %% 进入地图
-define(MAP_ROLE_MOVE_TYPE_LEAVE_MAP, 4).       %% 离开地图

%% 朝向
-define(MAP_ROLE_DIR_UP, 1).
-define(MAP_ROLE_DIR_UP_RIGHT, 2).
-define(MAP_ROLE_DIR_RIGHT, 3).
-define(MAP_ROLE_DIR_DOWN_RIGHT, 4).
-define(MAP_ROLE_DIR_DOWN, 5).
-define(MAP_ROLE_DIR_DOWN_LEFT, 6).
-define(MAP_ROLE_DIR_LEFT, 7).
-define(MAP_ROLE_DIR_UP_LEFT, 8).

%% 地图在线信息
-record(map, {
        %% 地图ID
        id = 1              :: pos_integer()
        %% 地图基础ID
        ,base_id = 1        :: pos_integer()
        %% 地图进程ID
        ,pid                :: undefined | pid()
        %% 宽度（像素）
        ,width = 1          :: pos_integer()
        %% 高度（像素）
        ,height = 1         :: pos_integer()
    }
).

%% 地图原始数据
-record(map_data, {
        %% 地图ID
        id = 1              :: pos_integer()
        %% 地图名称
        ,name = <<>>        :: bitstring()
        %% 宽度（像素）
        ,width = 1          :: pos_integer()
        %% 高度（像素）
        ,height = 1         :: pos_integer()
        %% 复活点 / 入口 [{X, Y}]
        ,revive = []         :: [{pos_integer(), pos_integer()}]
    }
).

%% 地图中的角色信息
-record(map_role, {
        %% 角色ID
        id = {0, <<>>, 0}   :: role_id()
        ,rid = 0
        ,platform = <<>>
        ,zone_id = 0
        %% 角色进程ID
        ,pid                 :: undefined | pid()
        %% 角色名称
        ,name = <<>>        :: bitstring()
        %% 连接器PID
        ,conn_pid           :: undefined | pid()
        %% 当前所在地图ID
        ,map = 0            :: non_neg_integer()
        %% 移动速度
        ,speed = 1          :: pos_integer()
        %% 当前X坐标
        ,x = 0              :: non_neg_integer()
        %% 当前Y坐标
        ,y = 0              :: non_neg_integer()
        %% 角色朝向
        ,dir = 0            :: non_neg_integer()
        %% 当前广播网格X坐标
        ,gx = 0             :: non_neg_integer()
        %% 当前广播网格Y坐标
        ,gy = 0             :: non_neg_integer()

        %% 角色状态
        ,status = 0         :: non_neg_integer()   
        %% 动作状态
        ,action = 0         :: non_neg_integer()
        %% 等级
        ,lev = 1            :: pos_integer()
        %% 当前血量
        ,hp = 1             :: non_neg_integer()
        %% 血量上限
        ,hp_max = 1         :: non_neg_integer()
        %% 当前法力
        ,mp = 1             :: non_neg_integer()
        %% 法力上限
        ,mp_max = 1         :: non_neg_integer()

        %% 最近一次移动
        ,last_move_src_x = 0    :: non_neg_integer()
        ,last_move_src_y = 0    :: non_neg_integer()
        ,last_move_dest_x = 0   :: non_neg_integer()
        ,last_move_dest_y = 0   :: non_neg_integer()
        ,last_move_dir = 0      :: non_neg_integer()
    }).

%% 角色移动信息
-record(map_role_move, {
        type = 0        :: non_neg_integer()
        ,args = {}      :: tuple()
    }
).
