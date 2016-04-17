%%----------------------------------------------------
%% 测试器相关数据结构
%%----------------------------------------------------
-ifdef(dbg_tester).
%% 输出调试信息
-define(T_DBG(Msg), logger:debug(Msg, [], ?MODULE, ?LINE)).
-define(T_DBG(F, A), logger:debug(F, A, ?MODULE, ?LINE)).
-else.
%% 停止输出调试信息
-define(T_DBG(Msg), ok).
-define(T_DBG(F, A), ok).
-endif.

-record(tester, {
        %% 角色ID
        id = 0                  :: 0 | {pos_integer(), bitstring(), non_neg_integer()}
        %% 测试器类型
        ,type = robot           :: player | robot
        %% 帐号名
        ,account = <<>>         :: bitstring()
        %% 平台标识
        ,platform = <<>>        :: bitstring()
        %% 区号
        ,zone_id = 1             :: pos_integer()
        %% 客户端连接类型
        ,client_type = 1        :: 1..3
        %% 角色名
        ,name = <<>>            :: bitstring()
        %% 等级
        ,lev = 1                :: pos_integer()
        %% 最大血量
        ,hp_max = 0             :: non_neg_integer()
        %% 当前血量
        ,hp = 0                 :: non_neg_integer()
        %% 移动速度（像素点/每秒）
        ,speed = 100            :: non_neg_integer()

        %% 位置
        ,map_id = 0             :: non_neg_integer()
        ,map_base_id = 0        :: non_neg_integer()
        ,x = 0                  :: non_neg_integer()
        ,y = 0                  :: non_neg_integer()
        ,dir = 0                :: non_neg_integer()
        ,width = 0              :: non_neg_integer()
        ,height = 0             :: non_neg_integer()

        %% 登陆检测
        ,login_check = 0         :: 0..1
        %% 当前连接的服务器
        ,object                 :: gateway | gameserver
        ,host                   :: string()
        ,port                   :: pos_integer()
        ,key                    :: string()
        ,ping                   :: pos_integer()
        %% 进程ID
        ,pid = 0
        ,socket
        ,connect_time
        ,read_head = false      %% 标识正在读取数据包头
    }
).
