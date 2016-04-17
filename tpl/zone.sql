
set sql_mode="no_auto_value_on_zero";

--
-- 帐号服务器数据库结构
--

--
-- 帐号数据
--
create table if not exists account (
    `platform` varchar(32) not null comment '平台标识',
    `acct_name` varchar(128) not null comment '帐号名',
    `password` varchar(128) not null comment '密码',
    `account` varchar(128) not null comment '玩家帐号ID，相当于UID',
    `client_type` int(11) not null comment '客户端类型',
    `device_id` varchar(128) not null default '' comment '设备号',
    `device_name` varchar(128) not null default '' comment '设备名',
    `reg_ip` char(16) DEFAULT NULL COMMENT '注册IP',
    `reg_time` int(11) not null default '0' comment '注册时间',
    primary key (platform, acct_name)
) engine=innodb default charset=utf8 comment='帐号数据';

--
-- 游戏节点数据库结构
--

--
-- 角色数据
--
create table if not exists role (
    `id` int(11) not null auto_increment,
    `platform` varchar(32) not null comment '平台标识',
    `zone_id` int(11) not null default '0' comment '区号',
    `account` varchar(128) not null comment '玩家帐号ID，相当于UID',
    `name` varchar(20) not null default '' comment '角色名',
    `lock_status` int(8) NOT NULL DEFAULT '0' COMMENT '账号锁定状态:0:正常,1封号2禁言3封号且禁言',
    `reg_ip` char(16) DEFAULT NULL COMMENT '注册IP',
    `reg_time` int(11) NOT NULL DEFAULT '0' COMMENT '注册时间',
    `login_ip` char(16) DEFAULT NULL COMMENT '最后一次登陆IP',
    `login_time` int(11) NOT NULL DEFAULT '0' COMMENT '登陆时间',
    `logout_time` int(11) NOT NULL DEFAULT '0' COMMENT '登出时间',
    `label` tinyint(4) not null default '0' comment '标志,0:玩家 1:GM 2:新手指导员',
    `sex` tinyint(4) not null default '0' comment '性别',
    `lev` tinyint(4) not null default '0' comment '等级',
    `ver` int(11) not null comment '版本号',
    `data` mediumblob NOT NULL COMMENT '序列化后的角色数据',
    primary key (id, platform, zone_id),
    unique acc (account, platform, zone_id),
    KEY `reg_time` (`reg_time`),
    KEY `logout_time` (`logout_time`),
    KEY `login_time` (`login_time`)
) engine=innodb default charset=utf8 comment='角色数据';

