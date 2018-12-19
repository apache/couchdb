/**
 * Licensed under the Apache License, Version 2.0 (the "License"); you may not
 * use this file except in compliance with the License. You may obtain a copy of
 * the License at
 *
 *   http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS, WITHOUT
 * WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied. See the
 * License for the specific language governing permissions and limitations under
 * the License.
 */

#include <stdio.h>
#include <assert.h>
#include "erl_nif.h"
#include "unicode/ucol.h"
#include "unicode/ucasemap.h"

#define MAX_DEPTH 10

#if (ERL_NIF_MAJOR_VERSION > 2) || \
    (ERL_NIF_MAJOR_VERSION == 2 && ERL_NIF_MINOR_VERSION >= 3)
/* OTP R15B or higher */
#define term_is_number(env, t) enif_is_number(env, t)
#else
#define term_is_number(env, t)  \
    (!enif_is_binary(env, t) && \
     !enif_is_list(env, t) &&   \
     !enif_is_tuple(env, t))
#endif

#ifdef _MSC_VER
#define threadlocal __declspec(thread)
#else
#define threadlocal __thread
#endif

static ERL_NIF_TERM ATOM_TRUE;
static ERL_NIF_TERM ATOM_FALSE;
static ERL_NIF_TERM ATOM_NULL;

typedef struct {
    ErlNifEnv* env;
    int error;
    UCollator* coll;
} ctx_t;

static threadlocal UCollator* collator = NULL;
static UCollator** collators = NULL;
static int numCollators = 0;
static int numSchedulers = 0;
static ErlNifMutex* collMutex = NULL;

static ERL_NIF_TERM less_json_nif(ErlNifEnv*, int, const ERL_NIF_TERM []);
static int on_load(ErlNifEnv*, void**, ERL_NIF_TERM);
static void on_unload(ErlNifEnv*, void*);
static __inline int less_json(int, ctx_t*, ERL_NIF_TERM, ERL_NIF_TERM);
static __inline int atom_sort_order(ErlNifEnv*, ERL_NIF_TERM);
static __inline int compare_strings(ctx_t*, ErlNifBinary, ErlNifBinary);
static __inline int compare_lists(int, ctx_t*, ERL_NIF_TERM, ERL_NIF_TERM);
static __inline int compare_props(int, ctx_t*, ERL_NIF_TERM, ERL_NIF_TERM);
static __inline UCollator* get_collator();


UCollator*
get_collator()
{
    UErrorCode status = U_ZERO_ERROR;

    if(collator != NULL) {
        return collator;
    }

    collator = ucol_open("", &status);

    if (U_FAILURE(status)) {
        ucol_close(collator);
        return NULL;
    }

    enif_mutex_lock(collMutex);
    collators[numCollators] = collator;
    numCollators++;
    enif_mutex_unlock(collMutex);

    assert(numCollators <= numSchedulers && "Number of schedulers shrank.");

    return collator;
}

ERL_NIF_TERM
less_json_nif(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    ctx_t ctx;
    int result;

    ctx.env = env;
    ctx.error = 0;
    ctx.coll = get_collator();

    result = less_json(1, &ctx, argv[0], argv[1]);

    /*
     * There are 2 possible failure reasons:
     *
     * 1) We got an invalid EJSON operand;
     * 2) The EJSON structures are too deep - to avoid allocating too
     *    many C stack frames (because less_json is a recursive function),
     *    and running out of memory, we throw a badarg exception to Erlang
     *    and do the comparison in Erlang land. In practice, views keys are
     *    EJSON structures with very little nesting.
     */
    return ctx.error ? enif_make_badarg(env) : enif_make_int(env, result);
}


int
less_json(int depth, ctx_t* ctx, ERL_NIF_TERM a, ERL_NIF_TERM b)
{
    int aIsAtom, bIsAtom;
    int aIsBin, bIsBin;
    int aIsNumber, bIsNumber;
    int aIsList, bIsList;
    int aArity, bArity;
    const ERL_NIF_TERM *aProps, *bProps;

    /*
     * Avoid too much recursion. Normally there isn't more than a few levels
     * of recursion, as in practice view keys do not go beyond 1 to 3 levels
     * of nesting. In case of too much recursion, signal it to the Erlang land
     * via an exception and do the EJSON comparison in Erlang land.
     */
    if (depth > MAX_DEPTH) {
        ctx->error = 1;
        return 0;
    }

    aIsAtom = enif_is_atom(ctx->env, a);
    bIsAtom = enif_is_atom(ctx->env, b);

    if (aIsAtom) {
        if (bIsAtom) {
            int aSortOrd, bSortOrd;

            if ((aSortOrd = atom_sort_order(ctx->env, a)) == -1) {
                ctx->error = 1;
                return 0;
            }

            if ((bSortOrd = atom_sort_order(ctx->env, b)) == -1) {
                ctx->error = 1;
                return 0;
            }

            return aSortOrd - bSortOrd;
        }

        return -1;
    }

    if (bIsAtom) {
        return 1;
    }

    aIsNumber = term_is_number(ctx->env, a);
    bIsNumber = term_is_number(ctx->env, b);

    if (aIsNumber) {
        if (bIsNumber) {
            return enif_compare(a, b);
        }

        return -1;
    }

    if (bIsNumber) {
        return 1;
    }

    aIsBin = enif_is_binary(ctx->env, a);
    bIsBin = enif_is_binary(ctx->env, b);

    if (aIsBin) {
        if (bIsBin) {
            ErlNifBinary binA, binB;

            enif_inspect_binary(ctx->env, a, &binA);
            enif_inspect_binary(ctx->env, b, &binB);

            return compare_strings(ctx, binA, binB);
        }

        return -1;
    }

    if (bIsBin) {
        return 1;
    }

    aIsList = enif_is_list(ctx->env, a);
    bIsList = enif_is_list(ctx->env, b);

    if (aIsList) {
        if (bIsList) {
            return compare_lists(depth, ctx, a, b);
        }

        return -1;
    }

    if (bIsList) {
        return 1;
    }

    if (!enif_get_tuple(ctx->env, a, &aArity, &aProps)) {
        ctx->error = 1;
        return 0;
    }
    if ((aArity != 1) || !enif_is_list(ctx->env, aProps[0])) {
        ctx->error = 1;
        return 0;
    }

    if (!enif_get_tuple(ctx->env, b, &bArity, &bProps)) {
        ctx->error = 1;
        return 0;
    }
    if ((bArity != 1) || !enif_is_list(ctx->env, bProps[0])) {
        ctx->error = 1;
        return 0;
    }

    return compare_props(depth, ctx, aProps[0], bProps[0]);
}


int
atom_sort_order(ErlNifEnv* env, ERL_NIF_TERM a)
{
    if (enif_compare(a, ATOM_NULL) == 0) {
        return 1;
    } else if (enif_compare(a, ATOM_FALSE) == 0) {
        return 2;
    } else if (enif_compare(a, ATOM_TRUE) == 0) {
        return 3;
    }

    return -1;
}


int
compare_lists(int depth, ctx_t* ctx, ERL_NIF_TERM a, ERL_NIF_TERM b)
{
    ERL_NIF_TERM headA, tailA;
    ERL_NIF_TERM headB, tailB;
    int aIsEmpty, bIsEmpty;
    int result;

    while (1) {
        aIsEmpty = !enif_get_list_cell(ctx->env, a, &headA, &tailA);
        bIsEmpty = !enif_get_list_cell(ctx->env, b, &headB, &tailB);

        if (aIsEmpty) {
            if (bIsEmpty) {
                return 0;
            }
            return -1;
        }

        if (bIsEmpty) {
            return 1;
        }

        result = less_json(depth + 1, ctx, headA, headB);

        if (ctx->error || result != 0) {
            return result;
        }

        a = tailA;
        b = tailB;
    }

    return result;
}


int
compare_props(int depth, ctx_t* ctx, ERL_NIF_TERM a, ERL_NIF_TERM b)
{
    ERL_NIF_TERM headA, tailA;
    ERL_NIF_TERM headB, tailB;
    int aArity, bArity;
    const ERL_NIF_TERM *aKV, *bKV;
    ErlNifBinary keyA, keyB;
    int aIsEmpty, bIsEmpty;
    int keyCompResult, valueCompResult;

    while (1) {
        aIsEmpty = !enif_get_list_cell(ctx->env, a, &headA, &tailA);
        bIsEmpty = !enif_get_list_cell(ctx->env, b, &headB, &tailB);

        if (aIsEmpty) {
            if (bIsEmpty) {
                return 0;
            }
            return -1;
        }

        if (bIsEmpty) {
            return 1;
        }

        if (!enif_get_tuple(ctx->env, headA, &aArity, &aKV)) {
            ctx->error = 1;
            return 0;
        }
        if ((aArity != 2) || !enif_inspect_binary(ctx->env, aKV[0], &keyA)) {
            ctx->error = 1;
            return 0;
        }

        if (!enif_get_tuple(ctx->env, headB, &bArity, &bKV)) {
            ctx->error = 1;
            return 0;
        }
        if ((bArity != 2) || !enif_inspect_binary(ctx->env, bKV[0], &keyB)) {
            ctx->error = 1;
            return 0;
        }

        keyCompResult = compare_strings(ctx, keyA, keyB);

        if (ctx->error || keyCompResult != 0) {
            return keyCompResult;
        }

        valueCompResult = less_json(depth + 1, ctx, aKV[1], bKV[1]);

        if (ctx->error || valueCompResult != 0) {
            return valueCompResult;
        }

        a = tailA;
        b = tailB;
    }

    return 0;
}


int
compare_strings(ctx_t* ctx, ErlNifBinary a, ErlNifBinary b)
{
    UErrorCode status = U_ZERO_ERROR;
    UCharIterator iterA, iterB;
    int result;

    uiter_setUTF8(&iterA, (const char *) a.data, (uint32_t) a.size);
    uiter_setUTF8(&iterB, (const char *) b.data, (uint32_t) b.size);

    result = ucol_strcollIter(ctx->coll, &iterA, &iterB, &status);

    if (U_FAILURE(status)) {
        ctx->error = 1;
        return 0;
    }

    /* ucol_strcollIter returns 0, -1 or 1
     * (see type UCollationResult in unicode/ucol.h) */

    return result;
}


int
on_load(ErlNifEnv* env, void** priv, ERL_NIF_TERM info)
{
    if (!enif_get_int(env, info, &numSchedulers)) {
        return 1;
    }

    if (numSchedulers < 1) {
        return 2;
    }

    collMutex = enif_mutex_create("coll_mutex");

    if (collMutex == NULL) {
        return 3;
    }

    collators = enif_alloc(sizeof(UCollator*) * numSchedulers);

    if (collators == NULL) {
        enif_mutex_destroy(collMutex);
        return 4;
    }

    ATOM_TRUE = enif_make_atom(env, "true");
    ATOM_FALSE = enif_make_atom(env, "false");
    ATOM_NULL = enif_make_atom(env, "null");

    return 0;
}


void
on_unload(ErlNifEnv* env, void* priv_data)
{
    if (collators != NULL) {
        int i;

        for (i = 0; i < numCollators; i++) {
            ucol_close(collators[i]);
        }

        enif_free(collators);
    }

    if (collMutex != NULL) {
        enif_mutex_destroy(collMutex);
    }
}


static ErlNifFunc nif_functions[] = {
    {"less_nif", 2, less_json_nif}
};


#ifdef __cplusplus
extern "C" {
#endif

ERL_NIF_INIT(couch_ejson_compare, nif_functions, &on_load, NULL, NULL, &on_unload);

#ifdef __cplusplus
}
#endif
