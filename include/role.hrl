%%----------------------------------------------------
%% 角色相关数据结构定义
%%----------------------------------------------------

%% 角色数据结构的当前版本号
-define(VER_ROLE, 1).

%% 活动事件
-define(event_normal,   0).             %% 当前无任何活动

%% 动作状态
-define(action_normal, 0).              %% 无动作
-define(action_combat, 1).              %% 战斗中

%% 角色状态
-define(status_normal, 0).              %% 正常

%% 角色数据
-record(role, {
        %% 角色数据结构版本号(这个位置必须在最前不要移动位置)
        ver = ?VER_ROLE             :: pos_integer()
        
        %% 角色ID，格式:{RoleId, Platform, ZoneId}
        %% （这里ver和id的位置就不要改动了！！！）
        ,id = {0, <<>>, 0}          :: role_id()
        %% 帐号ID
        ,account = <<>>             :: bitstring()
        %% 绑定账号/设备ID[如：苹果的推送token]
        ,bind_acc = <<>>            :: bitstring()
        %% 客户端类型[0:PC 1:APK 2:IPA]
        ,client = 0                 :: 0..2
        %% 角色名称
        ,name = <<>>                :: bitstring()
        %% 等级
        ,lev = 0                    :: 0..255
        %% 当前血量
        ,hp = 1                     :: non_neg_integer()
        %% 最高血量
        ,hp_max = 1                 :: non_neg_integer()
        %% 移动速度
        ,speed = 0                  :: non_neg_integer()

        %% 进程PID
        ,pid = 0                    :: 0 | pid()
        %% 状态
        ,status = 0                 :: 0..255
        %% 活动状态
        ,event = 0                  :: 0..255
        %% 动作状态
        ,action = 0               :: 0..255

        %% 内循环计数器(每次循环的间隔约为10秒)
        ,loop_cnt = 1               :: pos_integer()
        %% 同步需求标记
        ,need_sync = false          :: boolean()
        %% 是否记录该角色的所有rpc日志(调试控制项)
        ,debug_rpc = false          :: boolean()

        %% 连接属性: #link{}
        ,link                   :: tuple() | undefined
        
        %% 场景位置: #pos{}
        ,pos                        :: undefined | tuple()
        %% 扩展数据，用于记录一些特殊的扩展数据
        ,extra = []                 :: [{non_neg_integer(), term()}]
    }
).

%% 角色登录信息
-record(role_login_info, {
        id = {0, <<>>, 0}   :: role_id()
        ,account = <<>>     :: bitstring()
        ,pid = 0            :: 0 | pid()
    }
).

%% 角色在线缓存
-record(role_online_cache, {
        id = {0, <<>>, 0}   :: role_id()
        ,pid = 0            :: 0 | pid()
        ,name = <<>>        :: bitstring()
    }
).
