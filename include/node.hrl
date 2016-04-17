%%----------------------------------------------------
%% 节点信息数据结构
%%
%% @author wprehard@qq.com
%% @end
%%----------------------------------------------------

-type node_id() :: {bitstring(), pos_integer() | atom()}.

%% 节点信息结构
-record(node, {
        %% 节点ID：{<<"center">>, 0} | {<<"gateway">>, 0}
        id = {<<>>, 1}          :: node_id()
        %% 平台标识："center" 或者 平台名简写("uc")
        ,platform = <<>>        :: bitstring()
        %% 节点名
        ,name                   :: undefined | atom()
        %% 所在物理机器
        ,machine = 0            :: non_neg_integer()
        %% 游戏代号
        ,game_code = ""         :: string()
        %% 游戏平台("ios" / "android" / "pc")
        ,game_plat = ""         :: string()
        %% 节点类型
        ,type = zone            :: center | zone | gateway | cache
        %% 节点根目录，如: /game/zone/zone1 或 /game/zone/center
        ,zone_path = ""         :: string()
        %% 域名
        ,host = ""              :: string()
        %% IP
        ,ip = ""                :: string()
        %% 监听端口
        ,port = 8000            :: pos_integer()

        %% magic cookie
        ,cookie = "high_risk"   :: string()

        %% 开服时间戳(中央服默认0)
        ,open_time = 0          :: tuple()
        %% 数据库连接
        ,db_cfg = []            :: list()

        %% 是否是被合并的服
        ,is_merged = 0          :: 0 | 1
        %% 是否已合区
        ,is_merge = 0           :: 0 | 1
        %% 合区后指向的节点名
        ,merge_name             :: atom()
        %% 合区列表: [{P, Z}]
        ,merge_list = []        :: list()

        %% 是否启动后自动连接(中央服)
        ,auto_connect = true    :: boolean()
        %% 节点是否已经建立连接(已连接不代表已准备好)
        ,node_connected = false :: boolean()
        %% 镜像进程是否已经准备
        ,ready = 0              :: 0 | 1
        %% 代理镜像进程pid
        ,pid                    :: undefined | pid()
        %% 镜像进程pid(非中央服上的远端镜像)
        ,remote_pid             :: undefined | pid()
        %% monitor引用
        ,ref                    :: undefined | reference()
    }).

%% 中央服信息数据结构(本地节点保存)
-record(center, {
        %% 标识中央服是否连接
        ready = 0           :: 1 | 0
        %% 中央服镜像进程pid(远端)
        ,pid                :: undefined | pid()
        %% 中央服节点名
        ,node               :: atom()
        %% 用于快速本地访问
        %% 本地平台标识
        ,platform = <<>>    :: bitstring()
        %% 本地节点分区ID
        ,zone_id = <<>>      :: bitstring()
    }).

%% 游戏服分区信息数据结构(网关使用)
-record(game_zone, {
        %% 分区索引
        idx = {<<>>, 1}          :: node_id()
        %% 所属平台
        ,platform = <<>>        :: bitstring()
        %% 所属节点ID
        ,id = 1                 :: pos_integer()
        %% 分区ID
        ,svr_id = 0             :: non_neg_integer()
        %% 分区名
        ,svr_name = <<>>        :: bitstring()
        %% 分区标签
        ,svr_label = <<>>       :: bitstring()
        %% 分区状态[2-爆满 1-正常 0-停服]
        ,status = 1             :: 1 | 0
        %% 连接提示消息
        ,msg = <<>>             :: bitstring()
        %% 节点名
        ,name                   :: undefined | atom()
        %% host地址
        ,host = ""              :: string()
        %% IP地址
        ,ip = ""                :: string()
        %% 端口地址
        ,port = 8000            :: pos_integer()
        %% 当前玩家数
        ,reg_num = 0            :: non_neg_integer()
        %% 最大支持玩家数
        ,max_num = 0            :: non_neg_integer()
        %% 当前连接数
        ,link_num = 0           :: non_neg_integer()
        %% 最大支持连接数
        ,max_link = 1000        :: pos_integer()
        %% 验证server_key
        ,key = <<>>             :: string()
    }).
