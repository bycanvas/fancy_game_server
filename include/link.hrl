%%----------------------------------------------------
%% 连接器数据结构
%% 
%% @author yankai
%%----------------------------------------------------

%% 连接属性
-record(link, {
        %% socket
        socket              :: undefined | port()
        %% 连接器进程pid
        ,conn_pid           :: undefined | pid()
        %% 客户端IP地址
        ,ip = {0, 0, 0, 0}  :: {0..255, 0..255, 0..255, 0..255}
        %% 连接端口
        ,port = 0           :: non_neg_integer()
        %% 网络延时
        ,lag = 0            :: non_neg_integer()
    }
).
