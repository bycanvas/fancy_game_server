%%----------------------------------------------------
%% 条件数据结构定义
%%----------------------------------------------------

%% 条件数据结构
%% {kill_npc, eq, {1000, 3}}
%% {team_leader, true}
%% {has_friends, ge, 30}
%% {lev_equel, 30}
%% {lev_eq, 30}
%% {set_num, {blue, 3}}
-record(condition, {
        %% 标签
        label           :: atom()
        %% 比较运算符(默认为等于)，注意某些值只能使用eq运算符
        %% eq: 等于
        %% lt: 小于
        %% gt: 大于
        %% le: 小于等于
        %% ge: 大于等于
        %% range: 范围
        ,op = eq        :: ue | eq | lt | gt | le | ge | range
        %% 目标值,具体意义由相应的label决定
        ,val            :: term()
        %% 判定失败时的返回消息
        ,msg = <<>>     :: bitstring()
    }
).
