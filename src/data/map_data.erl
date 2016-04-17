%%==================================
%% @doc 由工具生成的数据文件，不需要手动修改
%%==================================
-module(map_data).
-compile(export_all).
-include("common.hrl").
-include("map.hrl").

all() ->[
 10000
].

startup() ->[
 10000
].

get(10000) -> #map_data{
	id = 10000
	,name = <<"平原"/utf8>>
	,width = 4500
	,height = 3000
    ,revive = [{2300,1400},{2500,1500},{2600,1600},{2400,1450},{2550, 1550}]};
get(_) -> error.

