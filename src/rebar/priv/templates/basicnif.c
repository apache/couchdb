#include "erl_nif.h"

static ErlNifResourceType* {{module}}_RESOURCE = NULL;

typedef struct
{
} {{module}}_handle;

// Prototypes
static ERL_NIF_TERM {{module}}_new(ErlNifEnv* env, int argc,
                                   const ERL_NIF_TERM argv[]);
static ERL_NIF_TERM {{module}}_myfunction(ErlNifEnv* env, int argc,
                                          const ERL_NIF_TERM argv[]);

static ErlNifFunc nif_funcs[] =
{
    {"new", 0, {{module}}_new},
    {"myfunction", 1, {{module}}_myfunction}
};

static ERL_NIF_TERM {{module}}_new(ErlNifEnv* env, int argc,
                                   const ERL_NIF_TERM argv[])
{
    {{module}}_handle* handle = enif_alloc_resource({{module}}_RESOURCE,
                                                    sizeof({{module}}_handle));
    ERL_NIF_TERM result = enif_make_resource(env, handle);
    enif_release_resource(handle);
    return enif_make_tuple2(env, enif_make_atom(env, "ok"), result);
}


static ERL_NIF_TERM {{module}}_myfunction(ErlNifEnv* env, int argc,
                                          const ERL_NIF_TERM argv[])
{
    return enif_make_atom(env, "ok");
}

static void {{module}}_resource_cleanup(ErlNifEnv* env, void* arg)
{
    /* Delete any dynamically allocated memory stored in {{module}}_handle */
    /* {{module}}_handle* handle = ({{module}}_handle*)arg; */
}

static int on_load(ErlNifEnv* env, void** priv_data, ERL_NIF_TERM load_info)
{
    ErlNifResourceFlags flags = ERL_NIF_RT_CREATE | ERL_NIF_RT_TAKEOVER;
    ErlNifResourceType* rt = enif_open_resource_type(env, NULL,
                                                     "{{module}}_resource",
                                                     &{{module}}_resource_cleanup,
                                                     flags, NULL);
    if (rt == NULL)
        return -1;

    {{module}}_RESOURCE = rt;

    return 0;
}

ERL_NIF_INIT({{module}}, nif_funcs, &on_load, NULL, NULL, NULL);
