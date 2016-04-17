%%----------------------------------------------------
%% 客户端RPC调用配置
%% 此文件由程序生成，不要手动修改
%%----------------------------------------------------
-module(rpc_cfg).
-export([
        get/1
        ,desc_fun/2
    ]
).

get(1101) ->
    {rpc,1101,false,false,false,[],{rpc_cfg,desc_fun,{req,1101}},undefined,{rpc_cfg,desc_fun,{reply,1101}},undefined,undefined,undefined};

get(1102) ->
    {rpc,1102,false,false,false,[],{rpc_cfg,desc_fun,{req,1102}},undefined,{rpc_cfg,desc_fun,{reply,1102}},undefined,undefined,undefined};

get(1103) ->
    {rpc,1103,false,false,false,[],{rpc_cfg,desc_fun,{req,1103}},undefined,{rpc_cfg,desc_fun,{reply,1103}},undefined,undefined,undefined};

get(1199) ->
    {rpc,1199,false,false,false,[],{rpc_cfg,desc_fun,{req,1199}},undefined,{rpc_cfg,desc_fun,{reply,1199}},undefined,undefined,undefined};

get(10100) ->
    {rpc,10100,false,false,false,[],{rpc_cfg,desc_fun,{req,10100}},undefined,{rpc_cfg,desc_fun,{reply,10100}},undefined,undefined,undefined};

get(10101) ->
    {rpc,10101,false,false,false,[],{rpc_cfg,desc_fun,{req,10101}},undefined,{rpc_cfg,desc_fun,{reply,10101}},undefined,undefined,undefined};

get(10102) ->
    {rpc,10102,false,false,false,[],{rpc_cfg,desc_fun,{req,10102}},undefined,{rpc_cfg,desc_fun,{reply,10102}},undefined,undefined,undefined};

get(10103) ->
    {rpc,10103,false,false,false,[],{rpc_cfg,desc_fun,{req,10103}},undefined,{rpc_cfg,desc_fun,{reply,10103}},undefined,undefined,undefined};

get(10120) ->
    {rpc,10120,false,false,false,[],{rpc_cfg,desc_fun,{req,10120}},undefined,{rpc_cfg,desc_fun,{reply,10120}},undefined,undefined,undefined};

get(10121) ->
    {rpc,10121,false,false,false,[],{rpc_cfg,desc_fun,{req,10121}},undefined,{rpc_cfg,desc_fun,{reply,10121}},undefined,undefined,undefined};

get(10150) ->
    {rpc,10150,false,false,false,[],{rpc_cfg,desc_fun,{req,10150}},undefined,{rpc_cfg,desc_fun,{reply,10150}},undefined,undefined,undefined};

get(10151) ->
    {rpc,10151,false,false,false,[],{rpc_cfg,desc_fun,{req,10151}},undefined,{rpc_cfg,desc_fun,{reply,10151}},undefined,undefined,undefined};

get(10152) ->
    {rpc,10152,false,false,false,[],{rpc_cfg,desc_fun,{req,10152}},undefined,{rpc_cfg,desc_fun,{reply,10152}},undefined,undefined,undefined};

get(10153) ->
    {rpc,10153,false,false,false,[],{rpc_cfg,desc_fun,{req,10153}},undefined,{rpc_cfg,desc_fun,{reply,10153}},undefined,undefined,undefined};

get(10000) ->
    {rpc,10000,false,false,false,[],{rpc_cfg,desc_fun,{req,10000}},undefined,{rpc_cfg,desc_fun,{reply,10000}},undefined,undefined,undefined};

get(1001) ->
    {rpc,1001,false,false,false,[],{rpc_cfg,desc_fun,{req,1001}},undefined,{rpc_cfg,desc_fun,{reply,1001}},undefined,undefined,undefined};

get(Code) ->
    {error, {rpc_cfg_undefined, Code}}.

desc_fun({req, 1101}, _Data)-> "查询角色列表";
desc_fun({reply, 1101}, _Data)-> "";
desc_fun({req, 1102}, _Data)-> "创建角色";
desc_fun({reply, 1102}, _Data)-> "";
desc_fun({req, 1103}, _Data)-> "登陆角色";
desc_fun({reply, 1103}, _Data)-> "";
desc_fun({req, 1199}, _Data)-> "心跳";
desc_fun({reply, 1199}, _Data)-> "";
desc_fun({req, 10100}, _Data)-> "请求进入地图";
desc_fun({reply, 10100}, _Data)-> "";
desc_fun({req, 10101}, _Data)-> "地图资源加载已完成，可以推送单位、角色列表等等信息";
desc_fun({reply, 10101}, _Data)-> "";
desc_fun({req, 10102}, _Data)-> "请求移动";
desc_fun({reply, 10102}, _Data)-> "";
desc_fun({req, 10103}, _Data)-> "移动过程中同步位置";
desc_fun({reply, 10103}, _Data)-> "";
desc_fun({req, 10120}, _Data)-> "";
desc_fun({reply, 10120}, _Data)-> "通知客户端角色已进入地图，加载地图资源";
desc_fun({req, 10121}, _Data)-> "";
desc_fun({reply, 10121}, _Data)-> "通知可视范围内的角色列表变化";
desc_fun({req, 10150}, _Data)-> "";
desc_fun({reply, 10150}, _Data)-> "广播进入可视范围/离开可视范围的角色列表";
desc_fun({req, 10151}, _Data)-> "";
desc_fun({reply, 10151}, _Data)-> "广播角色移动";
desc_fun({req, 10152}, _Data)-> "";
desc_fun({reply, 10152}, _Data)-> "广播角色离开地图";
desc_fun({req, 10153}, _Data)-> "";
desc_fun({reply, 10153}, _Data)-> "广播地图区域内角色信息变化";
desc_fun({req, 10000}, _Data)-> "初始化角色信息";
desc_fun({reply, 10000}, _Data)-> "";
desc_fun({req, 1001}, _Data)-> "登录区服务器";
desc_fun({reply, 1001}, _Data)-> "";
desc_fun(_, _) -> undefined.
