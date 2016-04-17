%%----------------------------------------------------
%% Acceptor监控树
%%----------------------------------------------------
-module(sup_acceptor).
-behaviour(supervisor).
-export([start_link/0, init/1]).
-include("common.hrl").

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
    ?INFO("[~w] 正在启动监控树...", [?MODULE]),
    {ok, {
            {simple_one_for_one, 50, 1},
            [
                {sys_acceptor, {sys_acceptor, start_link, []}, transient, 2000, worker, [sys_acceptor]}
            ]
        }
    }.
