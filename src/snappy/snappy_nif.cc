/**
 * Copyright 2011,  Filipe David Manana  <fdmanana@apache.org>
 * Web:  http://github.com/fdmanana/snappy-erlang-nif
 *
 * Licensed under the Apache License, Version 2.0 (the "License"); you may not
 * use this file except in compliance with the License. You may obtain a copy of
 * the License at
 *
 *  http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS, WITHOUT
 * WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied. See the
 * License for the specific language governing permissions and limitations under
 * the License.
 **/

#include <iostream>
#include <cstring>

#include "erl_nif_compat.h"
#include "google-snappy/snappy.h"
#include "google-snappy/snappy-sinksource.h"

#ifdef OTP_R13B03
#error OTP R13B03 not supported. Upgrade to R13B04 or later.
#endif


class SnappyNifSink : public snappy::Sink {
public:
    SnappyNifSink(ErlNifEnv* e) : env(e), length(0)  {
        if (!enif_alloc_binary_compat(env, 0, &bin)) {
            enif_release_binary_compat(env, &bin);
            throw std::bad_alloc();
        }
    }

    void Append(const char *data, size_t n) {
        if (data != reinterpret_cast<const char *>(bin.data + length)) {
            memcpy(bin.data + length, data, n);
        }
        length += n;
    }

    char* GetAppendBuffer(size_t len, char* scratch) {
        if ((length + len) > bin.size) {
            size_t sz = (len * 4) < 8192 ? 8192 : (len * 4);

            if (!enif_realloc_binary_compat(env, &bin, bin.size + sz)) {
                enif_release_binary_compat(env, &bin);
                throw std::bad_alloc();
            }
        }

        return reinterpret_cast<char *>(bin.data + length);
    }

    ErlNifBinary& getBin() {
        if (bin.size > length) {
            if (!enif_realloc_binary_compat(env, &bin, length)) {
                // shouldn't happen
                enif_release_binary_compat(env, &bin);
                throw std::bad_alloc();
            }
        }
        return bin;
    }

private:
    ErlNifEnv* env;
    ErlNifBinary bin;
    size_t length;
};


extern "C" {

    ERL_NIF_TERM snappy_compress(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]) {
        ErlNifBinary input;

        if (!enif_inspect_iolist_as_binary(env, argv[0], &input)) {
            return enif_make_badarg(env);
        }

        try {
            snappy::ByteArraySource source(reinterpret_cast<const char *>(input.data),
                                           input.size);
            SnappyNifSink sink(env);

            snappy::Compress(&source, &sink);

            return enif_make_tuple(env, 2,
                                   enif_make_atom(env, "ok"),
                                   enif_make_binary(env, &sink.getBin()));
        } catch(std::bad_alloc e) {
            return enif_make_tuple(env, 2,
                                   enif_make_atom(env, "error"),
                                   enif_make_atom(env, "insufficient_memory"));
        } catch(...) {
            return enif_make_tuple(env, 2,
                                   enif_make_atom(env, "error"),
                                   enif_make_atom(env, "unknown"));
        }
    }


    ERL_NIF_TERM snappy_decompress(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]) {
        ErlNifBinary input;

        if (!enif_inspect_iolist_as_binary(env, argv[0], &input)) {
            return enif_make_badarg(env);
        }

        try {
            size_t len = -1;
            bool isCompressed = snappy::GetUncompressedLength(
                reinterpret_cast<const char *>(input.data), input.size, &len);

            if (!isCompressed) {
                return enif_make_tuple(env, 2,
                                       enif_make_atom(env, "error"),
                                       enif_make_atom(env, "not_compressed_data"));
            }

            ErlNifBinary retBin;

            if (!enif_alloc_binary_compat(env, len, &retBin)) {
                return enif_make_tuple(env, 2,
                                       enif_make_atom(env, "error"),
                                       enif_make_atom(env, "insufficient_memory"));
            }

            bool valid = snappy::RawUncompress(reinterpret_cast<const char *>(input.data),
                                               input.size,
                                               reinterpret_cast<char *>(retBin.data));

            if (!valid) {
                return enif_make_tuple(env, 2,
                                       enif_make_atom(env, "error"),
                                       enif_make_atom(env, "corrupted_data"));
            }

            return enif_make_tuple(env, 2,
                                   enif_make_atom(env, "ok"),
                                   enif_make_binary(env, &retBin));
        } catch(...) {
            return enif_make_tuple(env, 2,
                                   enif_make_atom(env, "error"),
                                   enif_make_atom(env, "unknown"));
        }
    }


    ERL_NIF_TERM snappy_get_uncompressed_length(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]) {
        ErlNifBinary input;

        if (!enif_inspect_iolist_as_binary(env, argv[0], &input)) {
            return enif_make_badarg(env);
        }

        try {
            size_t len = -1;
            bool isCompressed = snappy::GetUncompressedLength(
                reinterpret_cast<const char *>(input.data), input.size, &len);

            if (isCompressed) {
                return enif_make_tuple(env, 2,
                                       enif_make_atom(env, "ok"),
                                       enif_make_ulong(env, len));
            } else {
                return enif_make_tuple(env, 2,
                                       enif_make_atom(env, "error"),
                                       enif_make_atom(env, "not_compressed_data"));
            }
        } catch(...) {
            return enif_make_tuple(env, 2,
                                   enif_make_atom(env, "error"),
                                   enif_make_atom(env, "unknown"));
        }
    }


    ERL_NIF_TERM snappy_is_valid_compressed_buffer(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]) {
        ErlNifBinary input;

        if (!enif_inspect_iolist_as_binary(env, argv[0], &input)) {
            return enif_make_badarg(env);
        }

        try {
            bool valid = snappy::IsValidCompressedBuffer(
                reinterpret_cast<const char *>(input.data), input.size);

            if (valid) {
                return enif_make_atom(env, "true");
            } else {
                return enif_make_atom(env, "false");
            }
        } catch(...) {
            return enif_make_tuple(env, 2,
                                   enif_make_atom(env, "error"),
                                   enif_make_atom(env, "unknown"));
        }
    }



    int on_load(ErlNifEnv* env, void** priv_data, ERL_NIF_TERM info) {
        return 0;
    }


    int on_reload(ErlNifEnv* env, void** priv_data, ERL_NIF_TERM info) {
        return 0;
    }

    int on_upgrade(ErlNifEnv* env, void** priv_data, void** old_data, ERL_NIF_TERM info) {
        return 0;
    }


    static ErlNifFunc nif_functions[] = {
        {"compress", 1, snappy_compress},
        {"decompress", 1, snappy_decompress},
        {"get_uncompressed_length", 1, snappy_get_uncompressed_length},
        {"is_valid_compressed_buffer", 1, snappy_is_valid_compressed_buffer}
    };

    ERL_NIF_INIT(snappy, nif_functions, &on_load, &on_reload, &on_upgrade, NULL);
}
