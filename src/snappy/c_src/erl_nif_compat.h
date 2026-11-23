/* Copyright (c) 2010-2011 Basho Technologies, Inc.
 * With some minor modifications by Filipe David Manana
 * <fdmanana@apache.org>
 *
 * This file is provided to you under the Apache License,
 * Version 2.0 (the "License"); you may not use this file
 * except in compliance with the License.  You may obtain
 * a copy of the License at
 *
 *   http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing,
 * software distributed under the License is distributed on an
 * "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
 * KIND, either express or implied.  See the License for the
 * specific language governing permissions and limitations
 * under the License.
*/

#ifndef ERL_NIF_COMPAT_H_
#define ERL_NIF_COMPAT_H_

#ifdef __cplusplus
extern "C" {
#endif /* __cplusplus */

#include "erl_nif.h"


#define enif_open_resource_type_compat(E, N, D, F, T) \
    enif_open_resource_type(E, NULL, N, D, F, T)

#define enif_alloc_resource_compat(E, T, S) \
    enif_alloc_resource(T, S)

#define enif_release_resource_compat(E, H) \
    enif_release_resource(H)

#define enif_alloc_binary_compat(E, S, B) \
    enif_alloc_binary(S, B)

#define enif_realloc_binary_compat(E, S, B) \
    enif_realloc_binary(S, B)

#define enif_release_binary_compat(E, B) \
    enif_release_binary(B)

#define enif_alloc_compat(E, S) \
    enif_alloc(S)

#define enif_free_compat(E, P) \
    enif_free(P)

#define enif_get_atom_compat(E, T, B, S) \
    enif_get_atom(E, T, B, S, ERL_NIF_LATIN1)

#define enif_priv_data_compat enif_priv_data
#define enif_make_string_compat enif_make_string
#define enif_make_existing_atom_compat enif_make_existing_atom
#define enif_make_uint_compat enif_make_uint


#ifdef __cplusplus
}
#endif /* __cplusplus */

#endif /* ERL_NIF_COMPAT_H_ */
