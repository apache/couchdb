// Licensed under the Apache License, Version 2.0 (the "License"); you may not
// use this file except in compliance with the License.  You may obtain a copy
// of the License at
//
//   http://www.apache.org/licenses/LICENSE-2.0
//
// Unless required by applicable law or agreed to in writing, software
// distributed under the License is distributed on an "AS IS" BASIS, WITHOUT
// WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.  See the
// License for the specific language governing permissions and limitations
// under the License.

#include <erl_nif.h>
#include <jq.h>
#include <string.h>

//------------------------------------------------------------------------------
// Resource definitions
//------------------------------------------------------------------------------
//
// To let us reuse a compiled jq program to evaluate on multiple input
// documents, we need to create a resource term to hold onto the `jq_state *`
// returned by `jq_init()`, and pass this pointer to `jq_teardown()` when
// Erlang releases the term.
//
// The value stored in this resource is a `jq_state *` pointer, so pointers
// into the resource's memory block are `jq_state **`.

static ErlNifResourceType *jq_resource;

static void jq_resource_destroy(ErlNifEnv *env, void *obj)
{
    jq_teardown((jq_state **)obj);
}

static int on_load(ErlNifEnv *env, void **priv_data, ERL_NIF_TERM load_info)
{
    jq_resource = enif_open_resource_type(env, "jq", "jq_resource",
            jq_resource_destroy,
            ERL_NIF_RT_CREATE | ERL_NIF_RT_TAKEOVER,
            NULL);

    return 0;
}

//------------------------------------------------------------------------------
// Erlang term helpers
//------------------------------------------------------------------------------

static ERL_NIF_TERM ok(ErlNifEnv *env, ERL_NIF_TERM value)
{
    return enif_make_tuple2(env, enif_make_atom(env, "ok"), value);
}

static ERL_NIF_TERM error(ErlNifEnv *env, char *message)
{
    return enif_make_tuple2(env,
            enif_make_atom(env, "error"),
            enif_make_string(env, message, ERL_NIF_LATIN1));
}

//------------------------------------------------------------------------------
// jq_compile/1
//------------------------------------------------------------------------------
//
// This compiles a jq program passed as an Erlang binary into a `jq_state`
// struct, and we store a pointer to this in a resource term that we return for
// later use with jq_eval/2.

static char *binary_to_cstr(ErlNifEnv *env, ERL_NIF_TERM term)
{
    ErlNifBinary binary;
    char *str = NULL;

    if (enif_inspect_binary(env, term, &binary)) {
        str = malloc(binary.size + 1);
        if (str != NULL) {
            memcpy(str, binary.data, binary.size);
            str[binary.size] = '\0';
        }
    }
    return str;
}

static ERL_NIF_TERM jq_compile_nif(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[])
{
    char *program = NULL;
    jq_state **jq_ptr = NULL;
    ERL_NIF_TERM ret;

    program = binary_to_cstr(env, argv[0]);
    if (program == NULL) {
        return error(env, "failed to transfer jq program");
    }

    jq_ptr = enif_alloc_resource(jq_resource, sizeof(jq_state *));
    if (jq_ptr == NULL) {
        free(program);
        return error(env, "failed to create resource object");
    }

    *jq_ptr = jq_init();

    if (jq_compile(*jq_ptr, program)) {
        ret = ok(env, enif_make_resource(env, jq_ptr));
    } else {
        ret = error(env, "failed to compile jq program");
    }

    enif_release_resource(jq_ptr);
    free(program);
    return ret;
}

//------------------------------------------------------------------------------
// Conversions between Erlang and native values
//------------------------------------------------------------------------------
//
// These functions convert between the Erlang representation of JSON values and
// the equivalent 'jv' values used by jq. The following schema is enforced on
// the Erlang representation:
//
// - Objects are represented as {[ {Key, Value}* ]} i.e. a 1-tuple containing a
//   list of pairs. Keys must be either atoms or binaries, both of which are
//   converted to strings. Values are any valid JSON value.
//
// - Arrays are represented as [Value*], i.e. a list of zero or more values.
//
// - Strings are represented as binaries, not Erlang strings. They're
//   transferred byte-for-byte into jv strings, which should be fine if the input
//   was UTF-8.
//
// - Because Erlang strings are just lists, they will be converted to JSON
//   arrays. An Erlang string used as an object key will cause conversion to
//   fail.
//
// - Erlang integers and floats are converted to jv numbers. The
//   `enif_get_int64()` function is used so that values up to the IEEE 754 max
//   safe int value of 2^53 - 1 are preserved. On conversion from jv, integers
//   with values less than 2^31 become Erlang ints, all other values become
//   floats.
//
// - The values `true`, `false` and `null` are represented by the atoms of the
//   same name.
//
// An input value not obeying these rules will cause the conversion to fail,
// indicated by the function returning 0. It may have partially written the
// converted value on failure, in which case this value must not be used.

static int term_is_key(ErlNifEnv *env, ERL_NIF_TERM term)
{
    return enif_is_atom(env, term) || enif_is_binary(env, term);
}

static int ejson_to_jv(ErlNifEnv *env, ERL_NIF_TERM term, jv *out, int is_key)
{
    jv key, value;
    int tuple_size = 0;
    unsigned int list_size = 0, i = 0;
    const ERL_NIF_TERM *items = NULL;
    ERL_NIF_TERM list, head;
    ErlNifBinary binary;
    ErlNifSInt64 num_int;
    double num_float = 0;
    char atom[256];

    if (enif_get_tuple(env, term, &tuple_size, &items)) {
        if (tuple_size != 1 || !enif_is_list(env, items[0])) {
            return 0;
        }

        *out = jv_object();
        list = items[0];

        while (enif_get_list_cell(env, list, &head, &list)) {
            if (!enif_get_tuple(env, head, &tuple_size, &items)) {
                return 0;
            }
            if (tuple_size != 2 || !term_is_key(env, items[0])) {
                return 0;
            }
            if (ejson_to_jv(env, items[0], &key, 1) && ejson_to_jv(env, items[1], &value, 0)) {
                *out = jv_object_set(*out, key, value);
            } else {
                return 0;
            }
        }
    } else if (enif_get_list_length(env, term, &list_size)) {
        *out = jv_array_sized(list_size);
        list = term;
        i = 0;

        while (enif_get_list_cell(env, list, &head, &list)) {
            if (ejson_to_jv(env, head, &value, 0)) {
                *out = jv_array_set(*out, i++, value);
            } else {
                return 0;
            }
        }
    } else if (enif_inspect_binary(env, term, &binary)) {
        *out = jv_string_sized((const char *)binary.data, binary.size);

    } else if (enif_get_int64(env, term, &num_int)) {
        *out = jv_number(num_int);

    } else if (enif_get_double(env, term, &num_float)) {
        *out = jv_number(num_float);

    } else if (enif_get_atom(env, term, atom, sizeof(atom), ERL_NIF_LATIN1)) {
        if (is_key) {
            if (enif_get_atom_length(env, term, &i, ERL_NIF_LATIN1)) {
                *out = jv_string_sized(atom, i);
            } else {
                return 0;
            }
        } else if (!strcmp(atom, "true")) {
            *out = jv_true();
        } else if (!strcmp(atom, "false")) {
            *out = jv_false();
        } else if (!strcmp(atom, "null")) {
            *out = jv_null();
        } else {
            return 0;
        }
    } else {
        return 0;
    }

    return 1;
}

// The `jv` type uses a refcounting system to manage lifetimes. JSON objects,
// arrays, strings and "invalid" values (returned when `jq_next()` runs out of
// results) are refcounted pointers, while the other types are inline numbers
// with no heap allocation.
//
// The refcount is managed using these functions:
//
// - `jv jv_copy(jv)`: increments the refcount and returns the input
//
// - `void jv_free(jv)`: decrements the refcount and frees the object's memory
//   on reaching zero
//
// Most of the `jv_*()` functions implement an ownership system where they
// "consume" i.e. `jv_free()` their inputs internally, so once you pass a value
// to them you should not reuse it. Conversely they "produce" their outputs
// i.e. they return values with positive refcounts. To signal that you're no
// longer using a `jv` value you must either pass it to another `jv_*()`
// function or `jv_free()` it.
//
// Detailed notes on the functions used are given below next to each value
// type. In general, functions that get an object's length consume the object,
// so it must be copied to avoid its data being freed before use. The iterator
// macros leave the object's refcount unchanged, but they copy the keys/values
// they yield, so those must be freed after use.
//
// `jv_to_ejson()` is written to keep the refcount of its input constant,
// rather than "consuming" the value, so it's up to the caller to call
// `jv_free()` afterward. This makes it safer to write expressions like
//
//      if (jv_to_ejson(env, key, &k) && jv_to_ejson(env, value, &v)) {
//          // ...
//      }
//
// If `jv_to_ejson()` were responsible for freeing its input, then a failure in
// converting `key` would result in `value` getting leaked. This has other
// benefits, such as avoiding freeing values before their containing object or
// array, and control flow like early returns is easier to deal with, since we
// don't need to free the input at all exits from this function.

static int jv_to_ejson(ErlNifEnv *env, jv json, ERL_NIF_TERM *out)
{
    int size = 0, i = 0, did_convert = 0;
    ERL_NIF_TERM *list = NULL, key, value;
    unsigned char *buf;

    switch (jv_get_kind(json)) {
        // memory management:
        //      - `jv_object_length()`: frees the object
        //      - `jv_object_foreach()` macro uses:
        //          - `jv_object_iter()`: calls `jv_object_iter_next()`
        //          - `jv_object_iter_next()`: does not copy or free
        //          - `jv_object_iter_valid()`: does not copy or free
        //          - `jv_object_iter_key()`: copies the key
        //          - `jv_object_iter_value()`: copies the value
        case JV_KIND_OBJECT:
            size = jv_object_length(jv_copy(json));
            list = calloc(size, sizeof(ERL_NIF_TERM));
            i = 0;

            if (list == NULL) {
                return 0;
            }

            jv_object_foreach(json, jv_key, jv_value) {
                did_convert = jv_to_ejson(env, jv_key, &key) && jv_to_ejson(env, jv_value, &value);
                jv_free(jv_key);
                jv_free(jv_value);

                if (did_convert && i < size) {
                    list[i++] = enif_make_tuple2(env, key, value);
                } else {
                    free(list);
                    return 0;
                }
            }
            *out = enif_make_tuple1(env, enif_make_list_from_array(env, list, size));
            free(list);
            break;

        // memory management:
        //      - `jv_array_length()`: frees the array
        //      - `jv_array_foreach()` macro uses:
        //          - `jv_array_length(jv_copy(a))`: copies then frees the array
        //          - `jv_array_get(jv_copy(a), i)`: copies the array, copies the slot, frees the array
        case JV_KIND_ARRAY:
            size = jv_array_length(jv_copy(json));
            list = calloc(size, sizeof(ERL_NIF_TERM));

            if (list == NULL) {
                return 0;
            }

            jv_array_foreach(json, idx, jv_value) {
                did_convert = jv_to_ejson(env, jv_value, &value);
                jv_free(jv_value);

                if (did_convert && idx < size) {
                    list[idx] = value;
                } else {
                    free(list);
                    return 0;
                }
            }
            *out = enif_make_list_from_array(env, list, size);
            free(list);
            break;

        // memory management:
        //      - `jv_string_length_bytes()`: frees the string
        //      - `jv_string_value()`: does not copy or free
        case JV_KIND_STRING:
            size = jv_string_length_bytes(jv_copy(json));
            buf = enif_make_new_binary(env, size, out);
            memcpy(buf, jv_string_value(json), size);
            break;

        // memory management: numbers are not refcounted
        case JV_KIND_NUMBER:
            if (jv_is_integer(json)) {
                *out = enif_make_int64(env, jv_number_value(json));
            } else {
                *out = enif_make_double(env, jv_number_value(json));
            }
            break;

        // memory management: `true` is a constant
        case JV_KIND_TRUE:
            *out = enif_make_atom(env, "true");
            break;

        // memory management: `false` is a constant
        case JV_KIND_FALSE:
            *out = enif_make_atom(env, "false");
            break;

        // memory management: `null` is a constant
        case JV_KIND_NULL:
            *out = enif_make_atom(env, "null");
            break;

        default:
            return 0;
    }

    return 1;
}

//------------------------------------------------------------------------------
// jq_eval/2
//------------------------------------------------------------------------------

static ERL_NIF_TERM jq_eval_nif(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[])
{
    jq_state **jq_ptr = NULL;
    jv doc = jv_null(), result = jv_null();
    int jq_flags = 0, did_convert = 0;
    ERL_NIF_TERM ret, item;

    if (!enif_get_resource(env, argv[0], jq_resource, (void **)&jq_ptr)) {
        return error(env, "failed to read compiled jq program");
    }

    if (!ejson_to_jv(env, argv[1], &doc, 0)) {
        jv_free(doc);
        return error(env, "failed to convert Erlang JSON value");
    }

    jq_start(*jq_ptr, doc, jq_flags);
    ret = enif_make_list(env, 0);

    while (1) {
        // calling `jq_next()` frees `doc`
        result = jq_next(*jq_ptr);

        if (!jv_is_valid(result)) {
            jv_free(result);
            break;
        }

        did_convert = jv_to_ejson(env, result, &item);
        jv_free(result);

        if (did_convert) {
            ret = enif_make_list_cell(env, item, ret);
        } else {
            return error(env, "failed to convert jv JSON value");
        }
    }
    return ok(env, ret);
}

//------------------------------------------------------------------------------
// NIF setup boilerplate
//------------------------------------------------------------------------------

static ErlNifFunc nif_funcs[] = {
    {"compile", 1, jq_compile_nif, 0},
    {"eval", 2, jq_eval_nif, 0}
};

ERL_NIF_INIT(couch_jq, nif_funcs, on_load, NULL, NULL, NULL);
