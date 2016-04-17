#include "erl_nif.h"
#include <string.h>

static ERL_NIF_TERM hello(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    int* ptr = NULL; // enif_alloc(1024*1024*10);
    *ptr = 100;
    enif_free(ptr);
        return enif_make_string(env, "Hello world!", ERL_NIF_LATIN1);
}

static ERL_NIF_TERM concat(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    char name[4096];
    enif_get_string(env, argv[0], name, sizeof(name), ERL_NIF_LATIN1);
    return enif_make_int(env, strlen(name));
}

static ERL_NIF_TERM alloc(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    int32_t mem;
    enif_get_int(env, argv[0], &mem);
    enif_alloc(mem);
    return enif_make_atom(env, "ok");
}

static ErlNifFunc nif_funcs[] =
{
        {"hello", 0, hello}
        ,{"concat", 1, concat}
        ,{"alloc", 1, alloc}
};

ERL_NIF_INIT(nif_util,nif_funcs,NULL,NULL,NULL,NULL)

