// This file is part of khash released under the MIT license.
// See the LICENSE file for more information.
// Copyright 2013 Cloudant, Inc <support@cloudant.com>

#include <assert.h>
#include <string.h>
#include <stdint.h>

#include "erl_nif.h"
#include "hash.h"

#ifdef _WIN32
#define INLINE __inline
#else
#define INLINE inline
#endif

#define KHASH_VERSION 0


typedef struct
{
    ERL_NIF_TERM atom_ok;
    ERL_NIF_TERM atom_error;
    ERL_NIF_TERM atom_value;
    ERL_NIF_TERM atom_not_found;
    ERL_NIF_TERM atom_end_of_table;
    ERL_NIF_TERM atom_expired_iterator;
    ErlNifResourceType* res_hash;
    ErlNifResourceType* res_iter;
} khash_priv;


typedef struct
{
    unsigned int hval;
    ErlNifEnv* env;
    ERL_NIF_TERM key;
    ERL_NIF_TERM val;
} khnode_t;


typedef struct
{
    int version;
    unsigned int gen;
    hash_t* h;
    ErlNifPid p;
} khash_t;


typedef struct
{
    int version;
    unsigned int gen;
    khash_t* khash;
    hscan_t scan;
} khash_iter_t;


static INLINE ERL_NIF_TERM
make_atom(ErlNifEnv* env, const char* name)
{
    ERL_NIF_TERM ret;
    if(enif_make_existing_atom(env, name, &ret, ERL_NIF_LATIN1)) {
        return ret;
    }
    return enif_make_atom(env, name);
}


static INLINE ERL_NIF_TERM
make_ok(ErlNifEnv* env, khash_priv* priv, ERL_NIF_TERM value)
{
    return enif_make_tuple2(env, priv->atom_ok, value);
}


static INLINE ERL_NIF_TERM
make_error(ErlNifEnv* env, khash_priv* priv, ERL_NIF_TERM reason)
{
    return enif_make_tuple2(env, priv->atom_error, reason);
}


static INLINE int
check_pid(ErlNifEnv* env, khash_t* khash)
{
    ErlNifPid pid;
    enif_self(env, &pid);

    if(enif_compare(pid.pid, khash->p.pid) == 0) {
        return 1;
    }

    return 0;
}


hnode_t*
khnode_alloc(void* ctx)
{
    hnode_t* ret = (hnode_t*) enif_alloc(sizeof(hnode_t));
    khnode_t* node = (khnode_t*) enif_alloc(sizeof(khnode_t));

    memset(ret, '\0', sizeof(hnode_t));
    memset(node, '\0', sizeof(khnode_t));

    node->env = enif_alloc_env();
    ret->hash_key = node;

   return ret;
}


void
khnode_free(hnode_t* obj, void* ctx)
{
    khnode_t* node = (khnode_t*) kl_hnode_getkey(obj);
    enif_free_env(node->env);
    enif_free(node);
    enif_free(obj);
    return;
}


int
khash_cmp_fun(const void* l, const void* r)
{
    khnode_t* left = (khnode_t*) l;
    khnode_t* right = (khnode_t*) r;
    int cmp = enif_compare(left->key, right->key);

    if(cmp < 0) {
        return -1;
    } else if(cmp == 0) {
        return 0;
    } else {
        return 1;
    }
}


hash_val_t
khash_hash_fun(const void* obj)
{
    khnode_t* node = (khnode_t*) obj;
    return (hash_val_t) node->hval;
}


static INLINE khash_t*
khash_create_int(ErlNifEnv* env, khash_priv* priv, ERL_NIF_TERM opts)
{
    khash_t* khash = NULL;

    assert(priv != NULL && "missing private data member");

    khash = (khash_t*) enif_alloc_resource(priv->res_hash, sizeof(khash_t));
    memset(khash, '\0', sizeof(khash_t));
    khash->version = KHASH_VERSION;
    khash->gen = 0;

    khash->h = kl_hash_create(HASHCOUNT_T_MAX, khash_cmp_fun, khash_hash_fun);

    if(khash->h == NULL ) {
        enif_release_resource(khash);
        return NULL;
    }

    kl_hash_set_allocator(khash->h, khnode_alloc, khnode_free, NULL);
    enif_self(env, &(khash->p));

    return khash;
}


static ERL_NIF_TERM
khash_new(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    khash_priv* priv = enif_priv_data(env);
    khash_t* khash;
    ERL_NIF_TERM ret;

    if(argc != 1) {
        return enif_make_badarg(env);
    }

    khash = khash_create_int(env, priv, argv[0]);
    if(khash == NULL) {
        return enif_make_badarg(env);
    }

    ret = enif_make_resource(env, khash);
    enif_release_resource(khash);

    return make_ok(env, priv, ret);
}


static void
khash_free(ErlNifEnv* env, void* obj)
{
    khash_t* khash = (khash_t*) obj;

    if(khash->h != NULL) {
        kl_hash_free_nodes(khash->h);
        kl_hash_destroy(khash->h);
    }

    return;
}


static ERL_NIF_TERM
khash_to_list(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    khash_priv* priv = (khash_priv*) enif_priv_data(env);
    ERL_NIF_TERM ret = enif_make_list(env, 0);
    khash_t* khash = NULL;
    void* res = NULL;
    hscan_t scan;
    hnode_t* entry;
    khnode_t* node;
    ERL_NIF_TERM key;
    ERL_NIF_TERM val;
    ERL_NIF_TERM tuple;

    if(argc != 1) {
        return enif_make_badarg(env);
    }

    if(!enif_get_resource(env, argv[0], priv->res_hash, &res)) {
        return enif_make_badarg(env);
    }

    khash = (khash_t*) res;

    if(!check_pid(env, khash)) {
        return enif_make_badarg(env);
    }

    kl_hash_scan_begin(&scan, khash->h);

    while((entry = kl_hash_scan_next(&scan)) != NULL) {
        node = (khnode_t*) kl_hnode_getkey(entry);
        key = enif_make_copy(env, node->key);
        val = enif_make_copy(env, node->val);
        tuple = enif_make_tuple2(env, key, val);
        ret = enif_make_list_cell(env, tuple, ret);
    }

    return ret;
}


static ERL_NIF_TERM
khash_clear(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    khash_priv* priv = enif_priv_data(env);
    khash_t* khash = NULL;
    void* res = NULL;

    if(argc != 1) {
        return enif_make_badarg(env);
    }

    if(!enif_get_resource(env, argv[0], priv->res_hash, &res)) {
        return enif_make_badarg(env);
    }

    khash = (khash_t*) res;

    if(!check_pid(env, khash)) {
        return enif_make_badarg(env);
    }

    kl_hash_free_nodes(khash->h);

    khash->gen += 1;

    return priv->atom_ok;
}


static INLINE hnode_t*
khash_lookup_int(ErlNifEnv* env, uint32_t hv, ERL_NIF_TERM key, khash_t* khash)
{
    khnode_t node;
    node.hval = hv;
    node.env = env;
    node.key = key;
    return kl_hash_lookup(khash->h, &node);
}


static ERL_NIF_TERM
khash_lookup(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    khash_priv* priv = enif_priv_data(env);
    khash_t* khash = NULL;
    void* res = NULL;
    uint32_t hval;
    hnode_t* entry;
    khnode_t* node;
    ERL_NIF_TERM ret;

    if(argc != 3) {
        return enif_make_badarg(env);
    }

    if(!enif_get_resource(env, argv[0], priv->res_hash, &res)) {
        return enif_make_badarg(env);
    }

    khash = (khash_t*) res;

    if(!check_pid(env, khash)) {
        return enif_make_badarg(env);
    }

    if(!enif_get_uint(env, argv[1], &hval)) {
        return enif_make_badarg(env);
    }

    entry = khash_lookup_int(env, hval, argv[2], khash);
    if(entry == NULL) {
        ret = priv->atom_not_found;
    } else {
        node = (khnode_t*) kl_hnode_getkey(entry);
        ret = enif_make_copy(env, node->val);
        ret = enif_make_tuple2(env, priv->atom_value, ret);
    }

    return ret;
}


static ERL_NIF_TERM
khash_get(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    khash_priv* priv = enif_priv_data(env);
    khash_t* khash = NULL;
    void* res = NULL;
    uint32_t hval;
    hnode_t* entry;
    khnode_t* node;
    ERL_NIF_TERM ret;

    if(argc != 4) {
        return enif_make_badarg(env);
    }

    if(!enif_get_resource(env, argv[0], priv->res_hash, &res)) {
        return enif_make_badarg(env);
    }

    khash = (khash_t*) res;

    if(!check_pid(env, khash)) {
        return enif_make_badarg(env);
    }

    if(!enif_get_uint(env, argv[1], &hval)) {
        return enif_make_badarg(env);
    }

    entry = khash_lookup_int(env, hval, argv[2], khash);
    if(entry == NULL) {
        ret = argv[3];
    } else {
        node = (khnode_t*) kl_hnode_getkey(entry);
        ret = enif_make_copy(env, node->val);
    }

    return ret;
}


static ERL_NIF_TERM
khash_put(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    khash_priv* priv = enif_priv_data(env);
    khash_t* khash = NULL;
    void* res = NULL;
    uint32_t hval;
    hnode_t* entry;
    khnode_t* node;

    if(argc != 4) {
        return enif_make_badarg(env);
    }

    if(!enif_get_resource(env, argv[0], priv->res_hash, &res)) {
        return enif_make_badarg(env);
    }

    khash = (khash_t*) res;

    if(!check_pid(env, khash)) {
        return enif_make_badarg(env);
    }

    if(!enif_get_uint(env, argv[1], &hval)) {
        return enif_make_badarg(env);
    }

    entry = khash_lookup_int(env, hval, argv[2], khash);
    if(entry == NULL) {
        entry = khnode_alloc(NULL);
        node = (khnode_t*) kl_hnode_getkey(entry);
        node->hval = hval;
        node->key = enif_make_copy(node->env, argv[2]);
        node->val = enif_make_copy(node->env, argv[3]);
        kl_hash_insert(khash->h, entry, node);
    } else {
        node = (khnode_t*) kl_hnode_getkey(entry);
        enif_clear_env(node->env);
        node->key = enif_make_copy(node->env, argv[2]);
        node->val = enif_make_copy(node->env, argv[3]);
    }

    khash->gen += 1;

    return priv->atom_ok;
}


static ERL_NIF_TERM
khash_del(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    khash_priv* priv = enif_priv_data(env);
    khash_t* khash = NULL;
    void* res = NULL;
    uint32_t hval;
    hnode_t* entry;
    ERL_NIF_TERM ret;

    if(argc != 3) {
        return enif_make_badarg(env);
    }

    if(!enif_get_resource(env, argv[0], priv->res_hash, &res)) {
        return enif_make_badarg(env);
    }

    khash = (khash_t*) res;

    if(!check_pid(env, khash)) {
        return enif_make_badarg(env);
    }

    if(!enif_get_uint(env, argv[1], &hval)) {
        return enif_make_badarg(env);
    }

    entry = khash_lookup_int(env, hval, argv[2], khash);
    if(entry == NULL) {
        ret = priv->atom_not_found;
    } else {
        kl_hash_delete_free(khash->h, entry);
        ret = priv->atom_ok;
    }

    khash->gen += 1;

    return ret;
}


static ERL_NIF_TERM
khash_size(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    khash_priv* priv = enif_priv_data(env);
    khash_t* khash;

    if(argc != 1) {
        return enif_make_badarg(env);
    }

    if(!enif_get_resource(env, argv[0], priv->res_hash, (void*) &khash)) {
        return enif_make_badarg(env);
    }

    if(!check_pid(env, khash)) {
        return enif_make_badarg(env);
    }

    return enif_make_uint64(env, kl_hash_count(khash->h));
}


static ERL_NIF_TERM
khash_iter(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    khash_priv* priv = enif_priv_data(env);
    khash_t* khash = NULL;
    void* res = NULL;
    khash_iter_t* iter;
    ERL_NIF_TERM ret;

    if(argc != 1) {
        return enif_make_badarg(env);
    }

    if(!enif_get_resource(env, argv[0], priv->res_hash, &res)) {
        return enif_make_badarg(env);
    }

    khash = (khash_t*) res;

    if(!check_pid(env, khash)) {
        return enif_make_badarg(env);
    }

    iter = (khash_iter_t*) enif_alloc_resource(
                priv->res_iter, sizeof(khash_iter_t));
    memset(iter, '\0', sizeof(khash_iter_t));
    iter->version = KHASH_VERSION;
    iter->gen = khash->gen;
    iter->khash = khash;
    kl_hash_scan_begin(&(iter->scan), iter->khash->h);

    // The iterator needs to guarantee that the khash
    // remains alive for the life of the iterator.
    enif_keep_resource(khash);

    ret = enif_make_resource(env, iter);
    enif_release_resource(iter);

    return make_ok(env, priv, ret);
}


static void
khash_iter_free(ErlNifEnv* env, void* obj)
{
    khash_iter_t* iter = (khash_iter_t*) obj;
    enif_release_resource(iter->khash);
}


static ERL_NIF_TERM
khash_iter_next(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    khash_priv* priv = enif_priv_data(env);
    khash_iter_t* iter = NULL;
    void* res = NULL;
    hnode_t* entry;
    khnode_t* node;
    ERL_NIF_TERM key;
    ERL_NIF_TERM val;

    if(argc != 1) {
        return enif_make_badarg(env);
    }

    if(!enif_get_resource(env, argv[0], priv->res_iter, &res)) {
        return enif_make_badarg(env);
    }

    iter = (khash_iter_t*) res;

    if(!check_pid(env, iter->khash)) {
        return enif_make_badarg(env);
    }

    if(iter->gen != iter->khash->gen) {
        return make_error(env, priv, priv->atom_expired_iterator);
    }

    entry = kl_hash_scan_next(&(iter->scan));
    if(entry == NULL) {
        return priv->atom_end_of_table;
    }

    node = (khnode_t*) kl_hnode_getkey(entry);
    key = enif_make_copy(env, node->key);
    val = enif_make_copy(env, node->val);
    return enif_make_tuple2(env, key, val);
}


static int
load(ErlNifEnv* env, void** priv, ERL_NIF_TERM info)
{
    int flags = ERL_NIF_RT_CREATE | ERL_NIF_RT_TAKEOVER;
    ErlNifResourceType* res;

    khash_priv* new_priv = (khash_priv*) enif_alloc(sizeof(khash_priv));
    if(new_priv == NULL) {
        return 1;
    }

    res = enif_open_resource_type(
            env, NULL, "khash", khash_free, flags, NULL);
    if(res == NULL) {
        return 1;
    }
    new_priv->res_hash = res;

    res = enif_open_resource_type(
            env, NULL, "khash_iter", khash_iter_free, flags, NULL);
    if(res == NULL) {
        return 1;
    }
    new_priv->res_iter = res;

    new_priv->atom_ok = make_atom(env, "ok");
    new_priv->atom_error = make_atom(env, "error");
    new_priv->atom_value = make_atom(env, "value");
    new_priv->atom_not_found = make_atom(env, "not_found");
    new_priv->atom_end_of_table = make_atom(env, "end_of_table");
    new_priv->atom_expired_iterator = make_atom(env, "expired_iterator");

    *priv = (void*) new_priv;

    return 0;
}


static int
reload(ErlNifEnv* env, void** priv, ERL_NIF_TERM info)
{
    return 0;
}


static int
upgrade(ErlNifEnv* env, void** priv, void** old_priv, ERL_NIF_TERM info)
{
    return load(env, priv, info);
}


static void
unload(ErlNifEnv* env, void* priv)
{
    enif_free(priv);
    return;
}


static ErlNifFunc funcs[] = {
    {"new", 1, khash_new},
    {"to_list", 1, khash_to_list},
    {"clear", 1, khash_clear},
    {"lookup_int", 3, khash_lookup},
    {"get_int", 4, khash_get},
    {"put_int", 4, khash_put},
    {"del_int", 3, khash_del},
    {"size", 1, khash_size},
    {"iter", 1, khash_iter},
    {"iter_next", 1, khash_iter_next}
};


ERL_NIF_INIT(khash, funcs, &load, &reload, &upgrade, &unload);
