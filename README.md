#fancy_game_server

#运行时版本：R17或以上

安装服务器节点：
mkdir -p zone/fancy_local_1
cp server/tpl/ctl.sh ./
编辑ctl.sh里的参数
sh ctl.sh install   （保证有MySQL的情况下）

启动节点：
sh ctl.sh start     （安装了screen）

