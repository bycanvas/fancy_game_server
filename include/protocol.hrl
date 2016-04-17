%%----------------------------------------------------
%% 模块配置数据结构
%%----------------------------------------------------
-record(module, {
        %% 模块代号
        code                :: pos_integer()
        %% 模块类型
        ,type               :: game_server | monitor | tester
        %% 是否需要验证
        ,need_auth = true   :: boolean()
        %% 调用者类型
        ,caller = connector :: connector | object
        %% 映射到的目标模块
        ,module             :: atom()
        %% 描述
        ,desc = ""          :: string()
    }
).

%% 客户端rpc调用配置数据
%% 协议格式说明:
%% {RecordName, Fields} RecordName:记录的名称 Fields:用于表示该记录中的项如何打包
%% 示例:
%  #rpc{
%%      reply = {role, [
%%              {uint32, id, "角色ID"}
%%              ,{string, name, "角色名称"}
%%          ]
%%      }
%% }
%% {int8, flag, "成功标志位"}
%% 有效类型: int8 uint8 int16 uint16 int32 uint32 float string array
%% 数组有以下四种表示方法，请选择合适的类型使用，不要做多余的转换:
%% 示例一(记录列表): {array, item, items, "物品列表", [
%%      {uint32, id, "物品ID"}
%%      ,{string, name, "物品名称"}
%% ]}
%% 接受的输入为: [#item{}, #item{}]
%%
%% 示例二(元组列表): {array, tuple, items, "物品列表", [
%%      {uint32, id, "物品ID"}
%%      ,{string, name, "物品名称"}
%% ]}
%% 接受的输入为: [{物品ID, 物品名称}, {物品ID, 物品名称}]
%%
%% 示例三(多项列表): {array, list, items, "物品列表", [
%%      {uint32, id, "物品ID"}
%%      ,{string, name, "物品名称"}
%% ]}
%% 接受的输入为: [[物品ID, 物品名称], [物品ID, 物品名称]]
%%
%% 示例四(单项列表): {array, single, items, "物品列表", [
%%      {uint32, id, "物品ID"}
%% ]}
%% 接受的输入为: [物品ID, 物品ID]
-record(rpc, {
        %% 协议号
        code = 0            :: non_neg_integer()
        %% 是否禁用
        ,disable = false    :: boolean()
        %% 是否自动记录日志
        ,autolog = false    :: boolean()
        %% 是否自动压缩(只会对reply的内容进行压缩，对request内容无效)
        ,autozip = false    :: boolean()
        %% 调用条件
        ,condition = []     :: [tuple()]
        %% 对该请求的文字描述(同时用于自动日志记录和调试信息显示)
        ,req_desc           :: string() | {string(), [atom()]} | {atom(), atom(), tuple()}
        %% 请求格式
        ,req = []           :: {atom(), [tuple()]} | [tuple()]
        %% 对返回结果的文字描述(同时用于自动日志记录和调试信息显示)
        ,reply_desc         :: string() | {string(), [atom()]} | {atom(), atom(), tuple()}
        %% 回应格式
        ,reply = []         :: {atom(), [tuple()]} | [tuple()]

        %% 协议解析器
        ,parser             :: atom()
        %% 梆定的调用处理模块
        ,mod                :: atom()
    }
).
