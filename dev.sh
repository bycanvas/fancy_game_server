#!/bin/bash
# ---------------------------------------------------------
# 开发工具脚本
# ---------------------------------------------------------

# 初始化
fun_init(){
    ROOT_PATH="$( cd -P "$( dirname "$SOURCE" )/.." && pwd )"
}

# 检测运行环境
in_cygwin(){
    os=$(uname)
    [[ "${os:0:3}" == "CYG" ]];
    return $?
}

# 获取依赖
fun_get_deps(){
    cd ${ROOT_PATH}/server
    ./rebar get-deps
}

# 编译项目
fun_make_server(){
    cd ${ROOT_PATH}/server
    ./rebar compile
}

fun_make_all(){
    fun_make_server
    fun_copy_apps
    fun_copy_deps
}

fun_copy_apps(){
    for file in ${ROOT_PATH}/server/apps/*
    do
        if test -d ${file}; then
            echo ">> 正在拷贝 ${file} 的beam和app文件"
            cp ${file}/ebin/*.beam ${ROOT_PATH}/server/ebin/
            cp ${file}/ebin/*.app ${ROOT_PATH}/server/ebin/
            echo ">> 拷贝完毕"
        fi
    done
}

fun_copy_deps(){
    for file in ${ROOT_PATH}/server/deps/*
    do
        if test -d ${file}; then
            echo ">> 正在拷贝 ${file} 的beam和app文件"
            cp ${file}/ebin/*.beam ${ROOT_PATH}/server/ebin/
            cp ${file}/ebin/*.app ${ROOT_PATH}/server/ebin/
            echo ">> 拷贝完毕"
        fi
    done
}


# 删除编译文件
fun_clean(){
    echo ">> 正在清理..."

    cd ${ROOT_PATH}/server
    ./rebar clean

    echo ">> 清理完毕"
}

## 执行dialyzer分析
fun_dialyzer(){
    echo ">> 正在执行dialyzer分析..."
    ## 删除这几个文件，不要分析
    rm -rf ${ROOT_PATH}/server/ebin/t.beam
    rm -rf ${ROOT_PATH}/server/ebin/test*.beam
    cd ${ROOT_PATH}/server
    dialyzer -I inc -r ebin
}

## 执行dialyzer单文件分析
fun_dialyzer_one(){
    echo ">> 正在执行dialyzer分析文件:$1..."
    cd ${ROOT_PATH}/server
    dialyzer -I inc -r ebin/$1*.beam
}

# 执行dialyzer初始化
fun_dialyzer_init(){
    echo ">> 正在执行dialyzer初始化..."
    dialyzer --build_plt --apps erts kernel stdlib crypto sasl edoc percept xmerl mnesia
}


## 编译NIF代码
fun_make_nif(){
    gcc -O3 -fPIC -shared -bundle -flat_namespace -undefined suppress -fno-common -Wall -o nif_util.so nif_util.c -I /usr/local/erlang/r17/lib/erlang/usr/include
}


fun_help(){
    echo "未知指令，请使用以下有效指令"
    echo "----------------------------------------------------------"
    echo "make          编译server源码"
    echo "make all      编译全部"
    echo "clean         清理ebin目录：beam、依赖库"
    echo "deps          获取依赖库"
    echo "dz_init       执行dialyzer分析的初始化"
    echo "dz_one        执行dialyzer单文件分析"
    echo "dz            执行dialyzer全局分析"
}

branch=$(git branch --no-color 2> /dev/null | sed -e '/^[^*]/d' -e 's/* \(.*\)/\1/')
echo "# 当前分支: ${branch}"
if [[ ${branch} == *master* ]];then
    echo ""
    echo "## 您正在发布分支操作，请留意"
    echo ""
fi

fun_init
case $1 in
    make)
        if [ "$2" == "all" ]; then
            fun_make_all
        elif [ "$2" == "nif" ]; then
            fun_make_nif
        else
            fun_make_server
        fi;;
    deps)
        fun_get_deps;;
    clean) fun_clean;;
    dz_init) fun_dialyzer_init;;
    dz_one) fun_dialyzer_one $2;;
    dz) fun_dialyzer;;
    *)
        fun_help;
        exit 1;;
esac
echo 
exit 0;
