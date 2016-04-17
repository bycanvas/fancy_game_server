%%----------------------------------------------------
%% 玩家数据版本转换
%%----------------------------------------------------
-module(role_ver).
-export([
        do/1
        ,do/2
    ]
).
-include("common.hrl").
-include("role.hrl").


%% @doc 转换玩家数据版本
-spec do(Data :: tuple()) -> {ok, #role{}} | {error, role_data_convert_failure}.
do(Data) ->
    %% #role{}的第2位项的必须是版本号
    %% ?DEBUG("Data = ~w",[Data]),
    case catch element(2, Data) of
        Ver when is_integer(Ver) ->
            do(Ver, Data);
        _ -> {error, role_data_convert_failure}
    end.

%% @doc 转换玩家数据版本
%% <div>
%% 注意：每次版本的控制, 版本Ver字段要自增1
%% 属于同个版本内容尽量在同个版本下转换
%% 每个版本的更新内容，请做好注释
%% </div>
-spec do(pos_integer(), #role{}) -> {ok, #role{}} | {error, role_data_convert_failure}.

do(Ver, Data) ->
    case Ver of
        ?VER_ROLE ->
            %% 模块更新
             %% @注意: item版本有更新，需要转换所有涉及的package
             Fields = [
                 
            ],
            do_field(Fields, Data#role{ver = Ver});
        _ ->
            ?ERR("角色数据版本转换失败:Ver=~w, Data=~w", [Ver, Data]),
            {error, role_data_convert_failure}
    end.
   
    

%% 检查子项数据版本并转换
do_field([], Data) ->
    {ok, Data};
do_field([Field | T], Data) ->
    case parse_field(Field, Data) of
        {error, E} -> {error, E};
        {ok, NewData} ->
            do_field(T, NewData)
    end.

%%----------------------------------------------------
%% 内部函数
%%----------------------------------------------------

parse_field(_F, _D) ->
    ?ERR("子项版本转换出错F:~w ~w", [_F, _D]),
    {error, field_ver_error}.
