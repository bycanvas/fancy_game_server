%%----------------------------------------------------
%% 连接器相关数据结构定义
%%----------------------------------------------------

-record(conn, {
        %% 控制对象 
        %% connector: 连接器自身
        %% object: 外部进程
        object              :: undefined | connector | role
        %% 连接器类型
        %% game_server: 游戏客户端
        %% monitor: 监控器
        %% tester: 测试器
        ,type = game_server :: game_server | monitor | tester
        %% 连接器绑定对象
        ,bind_obj           :: undefined | tuple()
        %% 连接器的所有者帐号ID
        %% ,account = <<>>     :: bitstring()
        %% 当前连接所属平台
        %% ,platform = <<>>    :: bitstring()
        %% 当前连接所属分区ID
        %% ,zone_id = 0        :: non_neg_integer()
        %% 当前连接端类型
        %% ,client = 0         :: 0..3
        %% 已登录的角色
        %% ,role_id = {0, <<>>, 0}:: {non_neg_integer(), non_neg_integer(), non_neg_integer()}
        %% 绑定对象pid
        ,bind_pid         :: undefined | pid()
        %% socket port
        ,socket             :: undefined | port()
        %% 客户端IP
        ,ip = {0, 0, 0, 0}  :: {0..255, 0..255, 0..255, 0..255}
        %% 客户端连接端口
        ,port = 1           :: pos_integer()
        %% 建立连接的时间
        ,connect_time = {0, 0, 0} :: {non_neg_integer(), non_neg_integer(), non_neg_integer()}
        %% 已接收的消息数量
        ,recv_count = 0     :: non_neg_integer()
        %% 最后一次检查时记录的已接收的消息数量
        ,last_recv_count = 0:: non_neg_integer()
        %% 已发送的消息数量
        %% ,send_count = 0     :: non_neg_integer()
        %% 发送错误次数
        ,bad_send_count = 0     :: non_neg_integer()
        %% 记录客户端发送的错误数据包个数
        ,bad_req_count = 0  :: non_neg_integer()

        %% 包体长度
        ,length = 0         :: non_neg_integer()
        %% 当前包序
        ,seq = 0            :: non_neg_integer()
        %% 标识正在读取数据包头
        ,read_head = false  :: boolean()
    }
).

%% %% 连接器配置
%% -record(conn_cfg, {
%%         %% 类型
%%         type        :: atom()
%%         %% 对应的socket
%%         ,socket     :: port()
%%         %% IP
%%         ,ip         :: tuple()
%%         %% 端口
%%         ,port = 0   :: non_neg_integer()
%%         %% 协议路由处理
%%         ,routing_m
%%         ,routing_f
%%     }
%% ).

%% 连接器定时任务
-record(conn_job, {
        %% 事件标签
        label               :: atom()
        %% 执行间隔（单位：毫秒）
        ,interval = 0       :: non_neg_integer()
        %% MFA
        ,m
        ,f
        ,a = []
        %% 是否马上执行
        ,at_once = false    :: boolean()
    }
).
