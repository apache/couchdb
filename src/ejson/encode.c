// Licensed under the Apache License, Version 2.0 (the "License"); you may not
// use this file except in compliance with the License. You may obtain a copy of
// the License at
//
//   http://www.apache.org/licenses/LICENSE-2.0
//
// Unless required by applicable law or agreed to in writing, software
// distributed under the License is distributed on an "AS IS" BASIS, WITHOUT
// WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied. See the
// License for the specific language governing permissions and limitations under
// the License.

#include <stdio.h>
#include <string.h>
#include <math.h>

#include "erl_nif.h"
#include "erl_nif_compat.h"
#include "yajl/yajl_encode.h"

#if defined(_WIN32) || defined(WIN32) || defined(__WIN32__)
#include <float.h>
#define isnan _isnan
#define isinf !_finite
#define snprintf _snprintf
#endif

#define SUCCESS 0
#define NOMEM 1
#define BADARG 2


typedef struct {
    ErlNifEnv* env;
    ErlNifBinary bin;
    size_t fill_offset;
    int error;
} encode_ctx;


static int
ensure_buffer(void* vctx, unsigned int len) {
    encode_ctx* ctx = (encode_ctx*)vctx;
    if ((ctx->bin.size - ctx->fill_offset) < len) {
        if(!enif_realloc_binary_compat(ctx->env, &(ctx->bin), (ctx->bin.size * 2) + len)) {
            return NOMEM;
        }
    }
    return SUCCESS;
}

static void
fill_buffer(void* vctx, const char* str, unsigned int len)
{
    encode_ctx* ctx = (encode_ctx*)vctx;

    if (ctx->error || (ctx->error = ensure_buffer(vctx, len))) {
        return;
    }
    memcpy(ctx->bin.data + ctx->fill_offset, str, len);
    ctx->fill_offset += len;
}

/* Json encode the string binary into the ctx.bin,
  with surrounding quotes and all */
static int
encode_string(void* vctx, ERL_NIF_TERM binary)
{
    encode_ctx* ctx = (encode_ctx*)vctx;
    ErlNifBinary bin;

    if(!enif_inspect_binary(ctx->env, binary, &bin)) {
        return NOMEM;
    }
    fill_buffer(ctx, "\"", 1);
    if (ctx->error) {
        return ctx->error;
    }
    yajl_string_encode2(fill_buffer, ctx, bin.data, bin.size);
    fill_buffer(ctx, "\"", 1);

    return ctx->error;
}

static ERL_NIF_TERM
no_mem_error(ErlNifEnv* env)
{
    return enif_make_tuple(env, 2,
            enif_make_atom(env, "error"),
            enif_make_atom(env, "insufficient_memory"));
}

ERL_NIF_TERM
final_encode(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    ERL_NIF_TERM head = argv[0];
    ERL_NIF_TERM term;
    double number;
    encode_ctx ctx;
    char* start;
    size_t len;
    size_t i;

    ctx.env = env;
    ctx.fill_offset = 0;
    ctx.error = 0;

    if (!enif_alloc_binary_compat(env, 100, &ctx.bin)) {
            return no_mem_error(env);
    }

    while(enif_get_list_cell(env, head, &term, &head)) {
        ErlNifBinary termbin;
        const ERL_NIF_TERM* array;
        int arity;
        int code;

        // We scan the list, looking for things to write into the binary, or
        // encode and then write into the binary. We encode values that are
        // tuples tagged with a type and a value: {Type, Value} where Type
        // is a an Integer and Value is what is to be encoded

        if (enif_get_tuple(env, term, &arity, &array)) {
            // It's a tuple to encode and copy
            if (arity != 2 || !enif_get_int(env, array[0], &code)) {
                // not arity 2 or the first element isn't an int
                ctx.error = BADARG;
                goto done;
            }
            if (code == 0) {
                // {0, String}
                if (encode_string(&ctx, array[1]) != SUCCESS) {
                    goto done;
                }
            }
            else {
                // {1, Double}
                if(!enif_get_double(env, array[1], &number)) {
                    ctx.error = BADARG;
                    goto done;
                }
                // We can't encode these.
                if (isnan(number) || isinf(number)) {
                    ctx.error = BADARG;
                    goto done;
                }
                if ((ctx.error = ensure_buffer(&ctx, 32)) != SUCCESS) {
                    goto done;
                }
                // write the string into the buffer
                start = (char*) (ctx.bin.data + ctx.fill_offset);
                snprintf(start, 32, "%0.20g", number);
                len = strlen(start);
                for(i = 0; i < len; i++) {
                    if(start[i] == '.' || start[i] == 'e' || start[i] == 'E') {
                        break;
                    }
                }
                if(i == len) {
                    if(i > 29) {
                        ctx.error = BADARG;
                        goto done;
                    }
                    start[len++] = '.';
                    start[len++] = '0';
                }
                // increment the length
                ctx.fill_offset += len;
            }
        } else if (enif_inspect_binary(env, term, &termbin)) {
            // this is a regular binary, copy the contents into the buffer
            fill_buffer(&ctx, (char*)termbin.data, termbin.size);
            if (ctx.error) {
                goto done;
            }
        }
        else {
            //not a binary, not a tuple, wtf!
            ctx.error = BADARG;
            goto done;
        }
    }
done:
    if (ctx.error == NOMEM) {
        enif_release_binary_compat(env, &ctx.bin);
        return no_mem_error(env);
    } else if (ctx.error == BADARG) {
        enif_release_binary_compat(env, &ctx.bin);
        return enif_make_badarg(env);
    }

    // Resize the binary to our exact final size
    if(!enif_realloc_binary_compat(env, &(ctx.bin), ctx.fill_offset)) {
        enif_release_binary_compat(env, &ctx.bin);
        return no_mem_error(env);
    }
    // make the binary term which transfers ownership
    return enif_make_binary(env, &ctx.bin);
}

