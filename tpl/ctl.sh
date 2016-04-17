#/bin/bash
# ---------------------------------------------------------
# 节点管理工具
# ---------------------------------------------------------

DEBUG=0 # 开发模式设置(0:关闭 1:开启)

# 初始化
init(){
    cfg_init
    cfg_check

    # 节点工作目录
    ROOT=${cfg['root']}
    # 代码库所处目录，例: /data/game.dev
    CODE_PATH=${cfg['code_path']}/${cfg['code_ver']}

    # erl程序所在路径
    # （在有多个版本运行时的服务器环境下，就需要指定路径）
    ERL=/usr/local/erlang/r17/bin/erl
    #（只有单一版本运行时的时候，只需要用默认的路径）
    # ERL=erl
    # erl节点间连接端口范围
    ERL_PORT_MIN=40100
    # erl节点间连接端口范围
    ERL_PORT_MAX=44000

    # 进入节点根目录
    cd ${ROOT} || exit 1
}

# 配置信息检查
cfg_check(){
    if [ -e "${cfg['root']}" ]; then
        return
    else if [ "${DEBUG}" != "1" ]; then
            echo ">> [错误]脚本未进行正确配置，如果希望以开发模式工作请将DEBUG设置为'1'，并填写关键配置"
            exit 1
        fi
    fi
    # 获取当前脚本文件所在路径
    SOURCE="${BASH_SOURCE[0]}"
    while [ -h "$SOURCE" ]; do
        local DIR="$( cd -P "$( dirname "$SOURCE" )" && pwd )"
        SOURCE="$(readlink "$SOURCE")"
        [[ $SOURCE != /* ]] && SOURCE="$DIR/$SOURCE"
    done
    SHELL_PATH="$( cd -P "$( dirname "$SOURCE" )" && pwd )"
    root=${SHELL_PATH}

    # 调试模式下的默认配置，如有必要请手动修改
    # cygwin下需使用dos格式才能正常，如:d:/game.dev/zone/sksy_local_1
    cfg['root']="${root}"
    # cygwin下需使用dos格式才能正常，如:d:/game.dev
    cfg['code_path']="/data/game.dev"
    cfg['nodename']="fy1@fancy.dev.local"
    cfg['platform']="dev"
    cfg['zone_id']="1"
    cfg['node_type']="zone"
    cfg['host']="fancy.dev.local"
    cfg['port']="15001"
    cfg['cookie']="abcdef"
    cfg['game_name']="fancy"
    cfg['zone_name']="本地开发服"
    cfg['lang']="zh_CN"
    cfg['db_host']="127.0.0.1"
    cfg['db_port']="3306"
    cfg['db_name']="fancy_local_1"
    cfg['db_user']="root"
    cfg['db_pass']="123456"
    cfg['db_conn_min']="10"
    cfg['db_conn_max']="30"
    cfg['ip']="192.168.1.188"

    # 替换空变量
    for k in ${!cfg[@]}; do
        cfg[$k]=${cfg[$k]/\{\{\{*\}\}\}/}
        #echo $k,${cfg[$k]}
    done
}

# 配置信息初始化
cfg_init(){
    # 节点根目录
    cfg['root']='{{{root}}}'
    # 代码存放路径，每一个版本一个目录保存到此路径下
    cfg['code_path']='{{{code_path}}}'
    # 平台分组
    cfg['platform_group']='{{{platform_group}}}'
    # 区号
    cfg['zone_id']='{{{zone_id}}}'
    # 节点类型
    cfg['node_type']='{{{node_type}}}'
    # magic cookie
    cfg['cookie']='{{{cookie}}}'
    # 服务器密钥
    cfg['srv_key']="{{{srv_key}}}"
    # 域名(这里不要带http://)
    cfg['host']='{{{host}}}'
    # 监听端口
    cfg['port']='{{{port}}}'
    # web监听端口
    cfg['web_port']='{{{web_port}}}'
    # 外网IP(一般为电信)
    cfg['ip']='{{{ip}}}'
    # 内网IP
    cfg['ip_internal']='{{{ip_internal}}}'
    # ssh用户名
    cfg['ssh_user']='{{{ssh_user}}}'
    # ssh访问端口
    cfg['ssh_port']='{{{ssh_port}}}'
    # 节点名称
    cfg['nodename']='{{{nodename}}}'
    # 数据库地址(如果该节点不使用数据库则留空)
    cfg['db_host']='{{{db_host}}}'
    # 数据库端口
    cfg['db_port']='{{{db_port}}}'
    # 数据库名称
    cfg['db_name']='{{{db_name}}}'
    # 数据库用户名
    cfg['db_user']='{{{db_user}}}'
    # 数据库密码
    cfg['db_pass']='{{{db_pass}}}'
    # 数据库最小连接数
    cfg['db_conn_min']='{{{db_conn_min}}}'
    # 数据库最大连接数
    cfg['db_conn_max']='{{{db_conn_max}}}'
    # 当前版本(此版本号用于标识当前版本，由安装工具自动填写)
    cfg['code_ver']='{{{code_ver}}}'
    # 游戏区名称
    cfg['zone_name']='{{{zone_name}}}'
    # 语言
    cfg['lang']="{{{lang}}}"
    # 合服信息
    cfg['combine']='{{{combine}}}'
    # 开服时间
    cfg['open_time']='{{{open_time}}}'
    # 合服时间
    cfg['merge_time']='{{{merge_time}}}'
    # 是否合服
    cfg['is_merge']='{{{is_merge}}}'
    # 是否自动连接中央服
    cfg['auto_connect']='{{{auto_connect}}}'
    # 大区服节点名称
    cfg['zone_center_nodename']='{{{zone_center_nodename}}}'
    # 大区服cookie
    cfg['zone_center_cookie']='{{{zone_center_cookie}}}'
    # 逻辑中央服节点名称
    cfg['center_nodename']="{{{center_nodename}}}"
    # 逻辑中央服cookie
    cfg['center_cookie']="{{{center_cookie}}}"
}

# 启动节点
fun_start(){
    ## cd ${SHELL_PATH}
    if [ ! -e dets ]; then
        echo ">> [错误]当前目录下未安装节点，请先执行安装操作"
        exit 1
    fi
    if $(in_cygwin); then
        werl -kernel inet_dist_listen_min ${ERL_PORT_MIN} -kernel inet_dist_listen_max ${ERL_PORT_MAX} +P 409600 +K true -smp enable -hidden -pa ${CODE_PATH}/server/ebin -name ${cfg['nodename']} -s main start -extra ${cfg['node_type']} &
    else
        start_file=${ROOT}/start.sh
        CMD="${ERL} -kernel inet_dist_listen_min ${ERL_PORT_MIN} -kernel inet_dist_listen_max ${ERL_PORT_MAX} +P 409600 +K true -smp enable +zdbbl 8192 -hidden -pa ${CODE_PATH}/server/ebin -name ${cfg['nodename']} -s main start -extra ${cfg['node_type']}"
        cat > ${start_file} <<EOF
#!/bin/bash
cd ${ROOT}
ulimit -SHn 102400
ulimit -SHc 409600
${CMD}
EOF
        chmod +x ${start_file}
        screen -dmSL ${cfg['nodename']} -s ${start_file}
        echo ">> 节点 ${cfg['nodename']} 正在启动中，如果想观察启动过程请使用以下命令进行启动:"
        echo ">> ./ctl.sh start && ./ctl.sh shell"
    fi
}

# 启动合服任务节点
fun_start_merge(){
    ## cd ${SHELL_PATH}
    if [ ! -e dets ]; then
        echo ">> [错误]当前目录下未安装节点，请先执行安装操作"
        exit 1
    fi
    if $(in_cygwin); then
        werl -kernel inet_dist_listen_min ${ERL_PORT_MIN} -kernel inet_dist_listen_max ${ERL_PORT_MAX} +P 409600 +K true -smp enable -hidden -pa ${CODE_PATH}/server/ebin -name ${cfg['nodename']} -s main start -merge_node true -extra ${cfg['node_type']} &
    else
        start_file=${ROOT}/start.sh
        CMD="${ERL} -kernel inet_dist_listen_min ${ERL_PORT_MIN} -kernel inet_dist_listen_max ${ERL_PORT_MAX} +P 409600 +K true -smp enable +zdbbl 8192 -hidden -pa ${CODE_PATH}/server/ebin -name ${cfg['nodename']} -s main start -merge_node true -extra ${cfg['node_type']}"
        cat > ${start_file} <<EOF
#!/bin/bash
cd ${ROOT}
ulimit -SHn 102400
ulimit -SHc 409600
${CMD}
EOF
        chmod +x ${start_file}
        screen -dmSL ${cfg['nodename']} -s ${start_file}
        echo ">> 节点 ${cfg['nodename']} 正在启动中，如果想观察启动过程请使用以下命令进行启动:"
        echo ">> ./ctl.sh start && ./ctl.sh shell"
    fi
}

# 关闭节点
fun_stop(){
    ## cd ${SHELL_PATH}
    if $(in_cygwin); then
        echo ">> cygwin下不支持此命令"
        exit
    fi
    ${ERL} -kernel inet_dist_listen_min ${ERL_PORT_MIN} -kernel inet_dist_listen_max ${ERL_PORT_MAX} -hidden -pa ${CODE_PATH}/server/ebin -name stop_${cfg['nodename']} -setcookie ${cfg['cookie']} -s main stop_from_shell -extra ${cfg['nodename']}
    echo "-------------------------------------------"
    echo ">> 节点 ${cfg['nodename']} 关闭完成"
}

# 使用remsh方式进入控制台
fun_remsh(){
    n=remsh_$1_${cfg['nodename']}
    ${ERL} -kernel inet_dist_listen_min ${ERL_PORT_MIN} -kernel inet_dist_listen_max ${ERL_PORT_MAX} -hidden -pa ${CODE_PATH}/server/ebin -name $n -setcookie ${cfg['cookie']} -remsh ${cfg['nodename']}
}

# 进入该节点的控制台
fun_shell(){
    if $(in_cygwin); then
        echo ">> cygwin下不支持此命令"
        exit
    fi
    screen -r "${cfg['nodename']}"
}

# 安装节点
fun_install(){
    echo ">> 节点安装开始"

    if [ -e dets ]; then
        echo ">> [错误]当前数据目录非空，可能是已经执行过安装过程，请删除后重试"
        exit 1
    fi
    # 处理模板
    tpls=( 'env_zone.cfg' 'emysql.app' 'main.app' 'zone.sql' )
    for v in ${tpls[*]};do
        file="${CODE_PATH}/server/tpl/${v}"
        if [ ! -e $file ]; then
            echo ">> [错误]找不到模板文件: ${file}"
            exit 1
        fi
        # 替换模板变量
        lines=$(<"${file}")
        for k in ${!cfg[@]}; do
            lines=${lines//\{\{\{$k\}\}\}/${cfg[$k]}}
        done
        # 替换空变量并生成文件
        echo "${lines}" | sed -e "s/{{{.*}}}//" > ${v}
    done
    # 安装数据库
    if [ "${cfg['db_name']}" != "" ]; then
        mysql -h${cfg['db_host']} -P${cfg['db_port']} -u${cfg['db_user']} -p${cfg['db_pass']} -e"create database ${cfg['db_name']}"
        if [ $? -ne 0 ]; then
            echo ">> [错误]创建数据库时发生异常"
            exit 1
        fi
        mysql -h${cfg['db_host']} -P${cfg['db_port']} -u${cfg['db_user']} -p${cfg['db_pass']} -D${cfg['db_name']} -e"source zone.sql"
        if [ $? -ne 0 ]; then
            echo ">> [错误]导入数据库表结构时发生异常"
            exit 1
        fi
        echo "安装数据库[${cfg['db_name']}]成功"
    else
        echo "此节点无须安装数据库"
    fi
    
    # 处理相关文件
    mkdir -p dets log/pack
    mv env_zone.cfg env.cfg
    rm -f zone.sql

    echo ">> 节点${cfg['nodename']}安装完成"
}

# 删除节点
fun_uninstall(){
    fun_check_screen

    # 删除数据库
    if [ "${cfg['db_name']}" != "" ]; then
        role_num=$(mysql -h${cfg['db_host']} -P${cfg['db_port']} -u${cfg['db_user']} -p${cfg['db_pass']} -D${cfg['db_name']} -ss -e"select count(*) from role")
        if [ $? -ne 0 ]; then
            echo ">> [错误]访问数据库失败，无法获取角色数量"
            exit 1
        fi
        if [ "$role_num" != "" ] && [ "$role_num" = "$1" ]; then
            mysql -h${cfg['db_host']} -P${cfg['db_port']} -u${cfg['db_user']} -p${cfg['db_pass']} -D${cfg['db_name']} -e"drop database ${cfg['db_name']}"
            if [ $? -ne 0 ]; then
                echo ">> [错误]访问数据库失败，无法删除数据库，如需继续请手动操作"
                exit 1
            fi
        else
            echo ">> [错误]删除参数中必须带有当前节点的角色数量，角色数量为[${role_num}]"
            exit 1
        fi
    fi
    
    rm -rf dets log env.cfg emysql.app main.app start.sh
    echo ">> 节点${cfg['nodename']}已经删除"
}

# 热更新节点
fun_update_cfg(){
    ## cd ${SHELL_PATH}
    # 处理模板
    tpls=( 'env_zone.cfg' 'emysql.app' 'main.app' )
    for v in ${tpls[*]};do
        file="${CODE_PATH}/server/tpl/${v}"
        # 替换模板变量
        lines=$(<"${file}")
        for k in ${!cfg[@]}; do
            lines=${lines//\{\{\{$k\}\}\}/${cfg[$k]}}
        done
        # 替换空变量并生成文件
        echo "${lines}" | sed -e "s/{{{.*}}}//" > ${v}
    done
    rm -f env.cfg && mv env_zone.cfg env.cfg
    echo ">> 节点${cfg['nodename']}所有配置文件更新完成"
}

# 清档操作
fun_clean(){
    fun_check_screen

    if [ "${cfg['db_name']}" != "" ]; then
        role_num=$(mysql -h${cfg['db_host']} -P${cfg['db_port']} -u${cfg['db_user']} -p${cfg['db_pass']} -D${cfg['db_name']} -ss -e"select count(*) from role")
        if [ $? -ne 0 ]; then
            echo ">> [错误]访问数据库失败，无法获取角色数量"
            exit 1
        fi
        if [ "$role_num" != "" ] && [ "$role_num" = "$1" ]; then
            # 排除列表，注意在排除列表外的所有表将会被清空
            exclude=( "media_card" )
            tabs=( $(mysql -h${cfg['db_host']} -P${cfg['db_port']} -u${cfg['db_user']} -p${cfg['db_pass']} -D${cfg['db_name']} -ss -e"show tables") )
            for v in ${tabs[@]}; do
                flag=0
                for v1 in ${exclude[@]}; do
                    if [ "$v" = "$v1" ]; then
                        flag=1
                    fi
                done
                if [ $flag = 0 ]; then
                    sql="${sql} truncate table ${v};"
                fi
            done
            mysql -h${cfg['db_host']} -P${cfg['db_port']} -u${cfg['db_user']} -p${cfg['db_pass']} -D${cfg['db_name']} -ss -e"${sql}"
            if [ $? -ne 0 ]; then
                echo ">> [错误]访问数据库失败"
                exit 1
            fi
            
        else
            echo ">> [错误]清档时参数中必须带有当前节点的角色数量，角色数量为[${role_num}]"
            exit 1
        fi
    fi
    
    rm -f dets/*
    rm -rf log
    rm -f screenlog.*
    mkdir -p log/pack
    echo ">> 清档操作完成"
}

# 检查对应的节点是否在运行
fun_check_screen() {
    if [ $(screen -ls | grep "${cfg['nodename']}" | wc -l ) -gt 0 ]; then 
        echo "服务器节点[${cfg['nodename']}]还在运行中，不能进行该操作"
        exit 1
    fi
}

# 收集并打包日志文件
fun_pack(){
    cd ${ROOT}/log/pack 
    # 如果目录非空则进行打包
    if [ $(ls -al | wc -l) -gt 3 ]; then
        tar zcf ${ROOT}/log/log_${cfg['platform_group']}_${cfg['zone_id']}_$(date +"%y%m%d%H%M%S").tar.gz *.txt && rm -f *.txt
        exit $? 
    fi
    echo "没有文件可打包"
    exit 1
}

## 批量导入激活码
fun_batch_import_code(){
    files=`ls ./ac_code/*.sql`
    for fn in ${files[*]};do
        echo ${fn}
        sql="source ${fn}"
        mysql -h${cfg['db_host']} -P${cfg['db_port']} -u${cfg['db_user']} -p${cfg['db_pass']} -D${cfg['db_name']} -ss -e"${sql}"
        if [ $? -ne 0 ]; then
            echo ">> [错误]导入激活码失败"
            exit 1
        fi
        echo "导入${fn}成功"
    done
    rm ./ac_code/*.sql
    echo "批量导入激活码成功"
}

## 检测是否在cygwin环境中
in_cygwin(){
    os=$(uname)
    [[ "${os:0:3}" == "CYG" ]]; return $?
}

## 命令行帮助
help(){
    echo "start             启动节点"
    echo "stop              关闭节点(同时会自动清理php session)"
    echo "shell             进入控制台，也可使用screen命令直接进入"
    echo "remsh             以remsh方式进入控制台"
    echo "install           安装节点"
    echo "uninstall         删除节点"
    echo "clean             清档操作"
    echo "update_cfg        更新所有配置文件"
    echo "pack              收集并打包日志文件"
    echo "log               查看控制台日志"
    echo "ls                列出所有的erl节点"
    echo "bat_import_code   批量导入激活码(比如:sh ctl.sh bat_import_code，会自动去当前目录的ac_code目录下导入所有的.sql文件)"
}

# ------------------------------------------------------
# 执行入口
# ------------------------------------------------------
declare -A cfg
init
case $1 in
    start)
        case $2 in
            merge) fun_start_merge;;
            *) fun_start;;
        esac
        ;;
    stop)  fun_stop;;
    shell) fun_shell;;
    remsh) fun_remsh $2;;
    install) fun_install;;
    uninstall) fun_uninstall $2;;
    clean) fun_clean $2;;
    update_cfg) fun_update_cfg;;
    pack) fun_pack;;
    log) less ./screenlog.0;;
    ls) screen -ls;;
    bat_import_code) fun_batch_import_code;;
    *)
        echo "未知指令，请使用以下有效指令"
        echo "----------------------------------------------------------"
        help
        exit 1
        ;;
esac
exit 0
