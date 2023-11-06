/*
 * fast-pbkdf2 - Optimal PBKDF2-HMAC calculation
 * Written in 2015 by Joseph Birr-Pixton <jpixton@gmail.com>
 *
 * To the extent possible under law, the author(s) have dedicated all
 * copyright and related and neighboring rights to this software to the
 * public domain worldwide. This software is distributed without any
 * warranty.
 *
 * You should have received a copy of the CC0 Public Domain Dedication
 * along with this software. If not, see
 * <http://creativecommons.org/publicdomain/zero/1.0/>.
 */

#include "erl_nif.h"

#ifndef TIMESLICE_PERCENTAGE
#define TIMESLICE_PERCENTAGE 5 // announce a timeslice of 5 percent when indicated
#define ITERS_PER_SLOT 6
/* On the single core of an 2,2 GHz Quad-Core Intel Core i7, in slightly below 1ms
 * we achieve around 120 iterations for sha512.
 * Also, we want to report percentage every 5% (TIMESLICE_PERCENTAGE).
 * We therefore get that a slot in between iterations should take 6 iterations (ITERS_PER_SLOT).
 */
#endif

#include <assert.h>
#include <string.h>
#include <stdlib.h>
#include <stdint.h>
#if defined(__GNUC__)
#include <sys/types.h>
#endif

#include <openssl/sha.h>

/* --- Common useful things --- */

static inline void write32_be(uint32_t n, uint8_t out[4])
{
#if defined(__GNUC__) && __GNUC__ >= 4 && __BYTE_ORDER == __LITTLE_ENDIAN
  *(uint32_t *)(out) = __builtin_bswap32(n);
#else
  out[0] = (n >> 24) & 0xff;
  out[1] = (n >> 16) & 0xff;
  out[2] = (n >> 8) & 0xff;
  out[3] = n & 0xff;
#endif
}

static inline void write64_be(uint64_t n, uint8_t out[8])
{
#if defined(__GNUC__) &&  __GNUC__ >= 4 && __BYTE_ORDER == __LITTLE_ENDIAN
  *(uint64_t *)(out) = __builtin_bswap64(n);
#else
  write32_be((n >> 32) & 0xffffffff, out);
  write32_be(n & 0xffffffff, out + 4);
#endif
}

/* Prepare block (of blocksz bytes) to contain md padding denoting a msg-size
 * message (in bytes).  block has a prefix of used bytes.
 * Message length is expressed in 32 bits (so suitable for all sha1 and sha2 algorithms). */
static inline void md_pad(uint8_t *block, size_t blocksz, size_t used, size_t msg)
{
  memset(block + used, 0, blocksz - used - 4);
  block[used] = 0x80;
  block += blocksz - 4;
  write32_be((uint32_t) (msg * 8), block);
}

/* Internal function/type names for hash-specific things. */
#define XSTRINGIFY(s) STRINGIFY(s)
#define STRINGIFY(s) #s

#define HMAC_CTX(_name) HMAC_ ## _name ## _ctx
#define HMAC_INIT(_name) HMAC_ ## _name ## _init
#define HMAC_UPDATE(_name) HMAC_ ## _name ## _update
#define HMAC_FINAL(_name) HMAC_ ## _name ## _final

#define HMAC_CTX_ROUND(_name) HMAC_ ## _name ## _ctx_round           // C struct
#define HMAC_CTX_ROUND_RES(_name) res_HMAC_ ## _name ## _ctx_round   // Erlang Resource definition
#define HMAC_CTX_ROUND_NAME(_name) XSTRINGIFY(HMAC_CTX_ROUND(_name)) // Erlang atom-name

#define PBKDF2_F(_name) pbkdf2_f_ ## _name
#define PBKDF2(_name) pbkdf2_ ## _name

typedef struct {
    ERL_NIF_TERM atom_sha;
    ERL_NIF_TERM atom_sha224;
    ERL_NIF_TERM atom_sha256;
    ERL_NIF_TERM atom_sha384;
    ERL_NIF_TERM atom_sha512;
    ErlNifResourceType* HMAC_CTX_ROUND_RES(sha1);
    ErlNifResourceType* HMAC_CTX_ROUND_RES(sha224);
    ErlNifResourceType* HMAC_CTX_ROUND_RES(sha256);
    ErlNifResourceType* HMAC_CTX_ROUND_RES(sha384);
    ErlNifResourceType* HMAC_CTX_ROUND_RES(sha512);
} pbkdf2_st;

/* This macro expands to decls for the whole implementation for a given
 * hash function.  Arguments are:
 *
 * _name like 'sha1', added to symbol names                         (e.g. sha256)
 * _blocksz block size, in bytes                                    (e.g. SHA256_CBLOCK)
 * _hashsz digest output, in bytes                                  (e.g. SHA256_DIGEST_LENGTH)
 * _ctx hash context type                                           (e.g. SHA256_Init)
 * _init hash context initialisation function                       (e.g. SHA256_Update)
 *    args: (_ctx *c)
 * _update hash context update function                             (e.g. SHA256_Update)
 *    args: (_ctx *c, const void *data, size_t ndata)
 * _final hash context finish function                              (e.g. SHA256_Final)
 *    args: (void *out, _ctx *c)
 * _xform hash context raw block update function                    (e.g. SHA256_Transform)
 *    args: (_ctx *c, const void *data)
 * _xcpy hash context raw copy function (only need copy hash state) (e.g. sha256_cpy)
 *    args: (_ctx * restrict out, const _ctx *restrict in)
 * _xtract hash context state extraction                            (e.g. sha256_extract)
 *    args: args (_ctx *restrict c, uint8_t *restrict out)
 * _xxor hash context xor function (only need xor hash state)       (e.g. sha256_xor)
 *    args: (_ctx *restrict out, const _ctx *restrict in)
 *
 * The resulting function is named PBKDF2(_name).
 */
#define DECL_PBKDF2(_name, _blocksz, _hashsz, _ctx,                           \
                    _init, _update, _xform, _final, _xcpy, _xtract, _xxor)    \
                                                                              \
  typedef struct {                                                            \
    _ctx inner;                                                               \
    _ctx outer;                                                               \
  } HMAC_CTX(_name);                                                          \
                                                                              \
  typedef struct {                                                            \
      HMAC_CTX(_name) startctx;                                               \
      HMAC_CTX(_name) ctx;                                                    \
      _ctx            result;                                                 \
      uint8_t         Ublock[_blocksz];                                       \
      uint32_t        iterations;                                             \
  } HMAC_CTX_ROUND(_name);                                                    \
                                                                              \
  static inline void HMAC_INIT(_name)(HMAC_CTX(_name) *ctx,                   \
                                      const uint8_t *key, size_t nkey)        \
  {                                                                           \
    /* Prepare key: */                                                        \
    uint8_t k[_blocksz];                                                      \
                                                                              \
    /* Shorten long keys. */                                                  \
    if (nkey > _blocksz)                                                      \
    {                                                                         \
      _init(&ctx->inner);                                                     \
      _update(&ctx->inner, key, nkey);                                        \
      _final(k, &ctx->inner);                                                 \
      key = k;                                                                \
      nkey = _hashsz;                                                         \
    }                                                                         \
                                                                              \
    /* Standard doesn't cover case where blocksz < hashsz. */                 \
    assert(nkey <= _blocksz);                                                 \
                                                                              \
    /* Right zero-pad short keys. */                                          \
    if (k != key)                                                             \
      memcpy(k, key, nkey);                                                   \
    if (_blocksz > nkey)                                                      \
      memset(k + nkey, 0, _blocksz - nkey);                                   \
                                                                              \
    /* Start inner hash computation */                                        \
    uint8_t blk_inner[_blocksz];                                              \
    uint8_t blk_outer[_blocksz];                                              \
                                                                              \
    for (size_t i = 0; i < _blocksz; i++)                                     \
    {                                                                         \
      blk_inner[i] = 0x36 ^ k[i];                                             \
      blk_outer[i] = 0x5c ^ k[i];                                             \
    }                                                                         \
                                                                              \
    _init(&ctx->inner);                                                       \
    _update(&ctx->inner, blk_inner, sizeof blk_inner);                        \
                                                                              \
    /* And outer. */                                                          \
    _init(&ctx->outer);                                                       \
    _update(&ctx->outer, blk_outer, sizeof blk_outer);                        \
  }                                                                           \
                                                                              \
  static inline void HMAC_UPDATE(_name)(HMAC_CTX(_name) *ctx,                 \
                                        const void *data, size_t ndata)       \
  {                                                                           \
    _update(&ctx->inner, data, ndata);                                        \
  }                                                                           \
                                                                              \
  static inline void HMAC_FINAL(_name)(HMAC_CTX(_name) *ctx,                  \
                                       uint8_t out[_hashsz])                  \
  {                                                                           \
    _final(out, &ctx->inner);                                                 \
    _update(&ctx->outer, out, _hashsz);                                       \
    _final(out, &ctx->outer);                                                 \
  }                                                                           \
                                                                              \
  /* --- PBKDF2 --- */                                                        \
  ERL_NIF_TERM PBKDF2_F(_name)(ErlNifEnv *env,                                \
                               int argc, const ERL_NIF_TERM argv[])           \
  {                                                                           \
    pbkdf2_st *mod_st = enif_priv_data(env);                                  \
    HMAC_CTX_ROUND(_name) *round_st;                                          \
    enif_get_resource(env, argv[0],                                           \
            mod_st->HMAC_CTX_ROUND_RES(_name),                                \
            ((void*) (&round_st)));                                           \
                                                                              \
    while (1) {                                                               \
        for (uint32_t i = 0; i < ITERS_PER_SLOT && i < round_st->iterations-1; ++i) \
        {                                                                     \
            /* Complete inner hash with previous U */                         \
            _xcpy(&round_st->ctx.inner, &round_st->startctx.inner);           \
            _xform(&round_st->ctx.inner, round_st->Ublock);                   \
            _xtract(&round_st->ctx.inner, round_st->Ublock);                  \
            /* Complete outer hash with inner output */                       \
            _xcpy(&round_st->ctx.outer, &round_st->startctx.outer);           \
            _xform(&round_st->ctx.outer, round_st->Ublock);                   \
            _xtract(&round_st->ctx.outer, round_st->Ublock);                  \
            _xxor(&round_st->result, &round_st->ctx.outer);                   \
        }                                                                     \
        if (round_st->iterations <= ITERS_PER_SLOT) break;                    \
        round_st->iterations -= ITERS_PER_SLOT;                               \
                                                                              \
        /* Schedule again but with iterations decremented */                  \
        if (enif_consume_timeslice(env, TIMESLICE_PERCENTAGE)) {              \
            return enif_schedule_nif(env, HMAC_CTX_ROUND_NAME(_name), 0,      \
                    PBKDF2_F(_name), argc, argv);                             \
        }                                                                     \
    }                                                                         \
                                                                              \
    /* We're done, so we can release the resource */                          \
    enif_release_resource(round_st);                                          \
    /* Reform result into output buffer. */                                   \
    ERL_NIF_TERM erl_result;                                                  \
    unsigned char *output = enif_make_new_binary(env, _hashsz, &erl_result);  \
    _xtract(&round_st->result, output);                                       \
    return erl_result;                                                        \
  }                                                                           \
                                                                              \
  static inline ERL_NIF_TERM PBKDF2(_name)(ErlNifEnv *env,                    \
                     const uint8_t *pw, size_t npw,                           \
                     const uint8_t *salt, size_t nsalt,                       \
                     uint32_t iterations, uint32_t counter)                   \
  {                                                                           \
    /* Retrieve the state resource descriptor from our priv data, */          \
    /* and allocate a new resource structure */                               \
    pbkdf2_st *mod_st = enif_priv_data(env);                                  \
    HMAC_CTX_ROUND(_name) *round_st = enif_alloc_resource(                    \
        mod_st->HMAC_CTX_ROUND_RES(_name),                                    \
        sizeof(HMAC_CTX_ROUND(_name))                                         \
    );                                                                        \
                                                                              \
    HMAC_INIT(_name)(&round_st->startctx, pw, npw);                           \
    uint8_t countbuf[4];                                                      \
    write32_be(counter, countbuf);                                            \
                                                                              \
    /* Prepare loop-invariant padding block. */                               \
    md_pad(round_st->Ublock, _blocksz, _hashsz, _blocksz + _hashsz);          \
                                                                              \
    /* First iteration:                                                       \
     *   U_1 = PRF(P, S || INT_32_BE(i))                                      \
     */                                                                       \
    round_st->ctx = round_st->startctx;                                       \
    HMAC_UPDATE(_name)(&round_st->ctx, salt, nsalt);                          \
    HMAC_UPDATE(_name)(&round_st->ctx, countbuf, sizeof countbuf);            \
    HMAC_FINAL(_name)(&round_st->ctx, round_st->Ublock);                      \
    round_st->result = round_st->ctx.outer;                                   \
    round_st->iterations = iterations;                                        \
                                                                              \
    ERL_NIF_TERM state_term = enif_make_resource(env, round_st);              \
    const ERL_NIF_TERM tmp_argv[] = {state_term};                             \
    return PBKDF2_F(_name)(env, 1, tmp_argv);                                 \
  }


static inline void sha1_extract(SHA_CTX *restrict ctx, uint8_t *restrict out)
{
  write32_be(ctx->h0, out);
  write32_be(ctx->h1, out + 4);
  write32_be(ctx->h2, out + 8);
  write32_be(ctx->h3, out + 12);
  write32_be(ctx->h4, out + 16);
}

static inline void sha1_cpy(SHA_CTX *restrict out, const SHA_CTX *restrict in)
{
  out->h0 = in->h0;
  out->h1 = in->h1;
  out->h2 = in->h2;
  out->h3 = in->h3;
  out->h4 = in->h4;
}

static inline void sha1_xor(SHA_CTX *restrict out, const SHA_CTX *restrict in)
{
  out->h0 ^= in->h0;
  out->h1 ^= in->h1;
  out->h2 ^= in->h2;
  out->h3 ^= in->h3;
  out->h4 ^= in->h4;
}

DECL_PBKDF2(sha1,
            SHA_CBLOCK,
            SHA_DIGEST_LENGTH,
            SHA_CTX,
            SHA1_Init,
            SHA1_Update,
            SHA1_Transform,
            SHA1_Final,
            sha1_cpy,
            sha1_extract,
            sha1_xor)

static inline void sha224_extract(SHA256_CTX *restrict ctx, uint8_t *restrict out)
{
  write32_be(ctx->h[0], out);
  write32_be(ctx->h[1], out + 4);
  write32_be(ctx->h[2], out + 8);
  write32_be(ctx->h[3], out + 12);
  write32_be(ctx->h[4], out + 16);
  write32_be(ctx->h[5], out + 20);
  write32_be(ctx->h[6], out + 24);
}

static inline void sha256_extract(SHA256_CTX *restrict ctx, uint8_t *restrict out)
{
  write32_be(ctx->h[0], out);
  write32_be(ctx->h[1], out + 4);
  write32_be(ctx->h[2], out + 8);
  write32_be(ctx->h[3], out + 12);
  write32_be(ctx->h[4], out + 16);
  write32_be(ctx->h[5], out + 20);
  write32_be(ctx->h[6], out + 24);
  write32_be(ctx->h[7], out + 28);
}

static inline void sha256_cpy(SHA256_CTX *restrict out, const SHA256_CTX *restrict in)
{
  out->h[0] = in->h[0];
  out->h[1] = in->h[1];
  out->h[2] = in->h[2];
  out->h[3] = in->h[3];
  out->h[4] = in->h[4];
  out->h[5] = in->h[5];
  out->h[6] = in->h[6];
  out->h[7] = in->h[7];
}

static inline void sha256_xor(SHA256_CTX *restrict out, const SHA256_CTX *restrict in)
{
  out->h[0] ^= in->h[0];
  out->h[1] ^= in->h[1];
  out->h[2] ^= in->h[2];
  out->h[3] ^= in->h[3];
  out->h[4] ^= in->h[4];
  out->h[5] ^= in->h[5];
  out->h[6] ^= in->h[6];
  out->h[7] ^= in->h[7];
}

DECL_PBKDF2(sha224,
            SHA256_CBLOCK,
            SHA224_DIGEST_LENGTH,
            SHA256_CTX,
            SHA224_Init,
            SHA224_Update,
            SHA256_Transform,
            SHA224_Final,
            sha256_cpy,
            sha224_extract,
            sha256_xor)

DECL_PBKDF2(sha256,
            SHA256_CBLOCK,
            SHA256_DIGEST_LENGTH,
            SHA256_CTX,
            SHA256_Init,
            SHA256_Update,
            SHA256_Transform,
            SHA256_Final,
            sha256_cpy,
            sha256_extract,
            sha256_xor)

static inline void sha384_extract(SHA512_CTX *restrict ctx, uint8_t *restrict out)
{
  write64_be(ctx->h[0], out);
  write64_be(ctx->h[1], out + 8);
  write64_be(ctx->h[2], out + 16);
  write64_be(ctx->h[3], out + 24);
  write64_be(ctx->h[4], out + 32);
  write64_be(ctx->h[5], out + 40);
}

static inline void sha512_extract(SHA512_CTX *restrict ctx, uint8_t *restrict out)
{
  write64_be(ctx->h[0], out);
  write64_be(ctx->h[1], out + 8);
  write64_be(ctx->h[2], out + 16);
  write64_be(ctx->h[3], out + 24);
  write64_be(ctx->h[4], out + 32);
  write64_be(ctx->h[5], out + 40);
  write64_be(ctx->h[6], out + 48);
  write64_be(ctx->h[7], out + 56);
}

static inline void sha512_cpy(SHA512_CTX *restrict out, const SHA512_CTX *restrict in)
{
  out->h[0] = in->h[0];
  out->h[1] = in->h[1];
  out->h[2] = in->h[2];
  out->h[3] = in->h[3];
  out->h[4] = in->h[4];
  out->h[5] = in->h[5];
  out->h[6] = in->h[6];
  out->h[7] = in->h[7];
}

static inline void sha512_xor(SHA512_CTX *restrict out, const SHA512_CTX *restrict in)
{
  out->h[0] ^= in->h[0];
  out->h[1] ^= in->h[1];
  out->h[2] ^= in->h[2];
  out->h[3] ^= in->h[3];
  out->h[4] ^= in->h[4];
  out->h[5] ^= in->h[5];
  out->h[6] ^= in->h[6];
  out->h[7] ^= in->h[7];
}

DECL_PBKDF2(sha384,
            SHA512_CBLOCK,
            SHA384_DIGEST_LENGTH,
            SHA512_CTX,
            SHA384_Init,
            SHA384_Update,
            SHA512_Transform,
            SHA384_Final,
            sha512_cpy,
            sha384_extract,
            sha512_xor)

DECL_PBKDF2(sha512,
            SHA512_CBLOCK,
            SHA512_DIGEST_LENGTH,
            SHA512_CTX,
            SHA512_Init,
            SHA512_Update,
            SHA512_Transform,
            SHA512_Final,
            sha512_cpy,
            sha512_extract,
            sha512_xor)

static int load(ErlNifEnv* env, void** priv_data, ERL_NIF_TERM load_info)
{
    (void) load_info;
    pbkdf2_st* mod_st = enif_alloc(sizeof(pbkdf2_st));
    if(mod_st == NULL) {
        return 1;
    }

    mod_st->atom_sha = enif_make_atom(env, "sha");
    mod_st->atom_sha224 = enif_make_atom(env, "sha224");
    mod_st->atom_sha256 = enif_make_atom(env, "sha256");
    mod_st->atom_sha384 = enif_make_atom(env, "sha384");
    mod_st->atom_sha512 = enif_make_atom(env, "sha512");

    mod_st->HMAC_CTX_ROUND_RES(sha1) = enif_open_resource_type(
            env, NULL, HMAC_CTX_ROUND_NAME(sha1),
            NULL, ERL_NIF_RT_CREATE | ERL_NIF_RT_TAKEOVER, NULL
        );

    mod_st->HMAC_CTX_ROUND_RES(sha224) = enif_open_resource_type(
            env, NULL, HMAC_CTX_ROUND_NAME(sha224),
            NULL, ERL_NIF_RT_CREATE | ERL_NIF_RT_TAKEOVER, NULL
        );

    mod_st->HMAC_CTX_ROUND_RES(sha256) = enif_open_resource_type(
            env, NULL, HMAC_CTX_ROUND_NAME(sha256),
            NULL, ERL_NIF_RT_CREATE | ERL_NIF_RT_TAKEOVER, NULL
        );

    mod_st->HMAC_CTX_ROUND_RES(sha384) = enif_open_resource_type(
            env, NULL, HMAC_CTX_ROUND_NAME(sha384),
            NULL, ERL_NIF_RT_CREATE | ERL_NIF_RT_TAKEOVER, NULL
        );

    mod_st->HMAC_CTX_ROUND_RES(sha512) = enif_open_resource_type(
            env, NULL, HMAC_CTX_ROUND_NAME(sha512),
            NULL, ERL_NIF_RT_CREATE | ERL_NIF_RT_TAKEOVER, NULL
        );

    *priv_data = (void*) mod_st;

    return 0;
}

static int reload(ErlNifEnv* env, void** priv, ERL_NIF_TERM info)
{
    return 0;
}

static int upgrade(ErlNifEnv* env, void** priv, void** old_priv, ERL_NIF_TERM info)
{
    return load(env, priv, info);
}

static void unload(ErlNifEnv* env, void* priv)
{
    enif_free(priv);
    return;
}

ERL_NIF_TERM mk_error(ErlNifEnv* env, const char *error_msg)
{
    return enif_make_tuple2(
            env,
            enif_make_atom(env, "error"),
            enif_make_atom(env, error_msg)
            );
}

static ERL_NIF_TERM
pbkdf2_nif(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    if(argc != 5)
        return enif_make_badarg(env);

    ErlNifBinary password;
    if (!enif_inspect_binary(env, argv[1], &password))
        return mk_error(env, "bad_password");

    ErlNifBinary salt;
    if (!enif_inspect_binary(env, argv[2], &salt))
        return mk_error(env, "bad_salt");

    int iteration_count;
    if (!enif_get_int(env, argv[3], &iteration_count))
        return mk_error(env, "bad_iteration_count");
    if (iteration_count <= 0)
        return mk_error(env, "bad_iteration_count");

    int counter;
    if (!enif_get_int(env, argv[4], &counter))
        return mk_error(env, "bad_block_counter");
    if (counter <= 0)
        return mk_error(env, "bad_block_counter");

    /** Calculates PBKDF2-HMAC-SHA
     *  @p npw bytes at @p pw are the password input.
     *  @p nsalt bytes at @p salt are the salt input.
     *  @p iterations is the PBKDF2 iteration count and must be non-zero.
     */
    pbkdf2_st *mod_st = (pbkdf2_st*) enif_priv_data(env);

    if(enif_is_identical(argv[0], mod_st->atom_sha)) {
        return PBKDF2(sha1)(env,
                password.data, password.size,
                salt.data, salt.size,
                iteration_count, counter);
    } else if(enif_is_identical(argv[0], mod_st->atom_sha224)) {
        return PBKDF2(sha224)(env,
                password.data, password.size,
                salt.data, salt.size,
                iteration_count, counter);
    } else if(enif_is_identical(argv[0], mod_st->atom_sha256)) {
        return PBKDF2(sha256)(env,
                password.data, password.size,
                salt.data, salt.size,
                iteration_count, counter);
    } else if(enif_is_identical(argv[0], mod_st->atom_sha384)) {
        return PBKDF2(sha384)(env,
                password.data, password.size,
                salt.data, salt.size,
                iteration_count, counter);
    } else if(enif_is_identical(argv[0], mod_st->atom_sha512)) {
        return PBKDF2(sha512)(env,
                password.data, password.size,
                salt.data, salt.size,
                iteration_count, counter);
    } else {
        return mk_error(env, "bad_hash");
    }
}

static ErlNifFunc fastpbkdf2_nif_funcs[] = {
    {"pbkdf2_block", 5, pbkdf2_nif}
};

ERL_NIF_INIT(fast_pbkdf2, fastpbkdf2_nif_funcs, load, reload, upgrade, unload);
