// Licensed under the Apache License, Version 2.0 (the "License"); you may not
// use this file except in compliance with the License. You may obtain a copy of
// the License at
//
// http://www.apache.org/licenses/LICENSE-2.0
//
// Unless required by applicable law or agreed to in writing, software
// distributed under the License is distributed on an "AS IS" BASIS, WITHOUT
// WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied. See the
// License for the specific language governing permissions and limitations under
// the License.

#include "erl_nif.h"
#include "xxhash.h"

static ERL_NIF_TERM xxhash128_nif(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
   ErlNifBinary bin;
   if(!enif_inspect_binary(env, argv[0], &bin)) {
       return enif_make_badarg(env);
   }
   XXH128_hash_t h = XXH3_128bits((const char*) bin.data, (const size_t) bin.size);
   return enif_make_tuple2(env, enif_make_uint64(env, h.high64), enif_make_uint64(env, h.low64));
}

static ErlNifFunc funcs[] = {
    {"xxhash128_plain_nif", 1, xxhash128_nif},
    {"xxhash128_dirty_nif", 1, xxhash128_nif, ERL_NIF_DIRTY_JOB_CPU_BOUND}
};

ERL_NIF_INIT(exxhash, funcs, NULL, NULL, NULL, NULL);
