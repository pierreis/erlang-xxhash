#include <stdint.h>

#include "erl_nif.h"
#define XXH_STATIC_LINKING_ONLY
#include "xxhash.h"

#define UNUSED(x) (void)(x)


// Misc. definitions

void xxhash_cleanup(ErlNifEnv *env, void *p);


// Erlang terms and exports.

static ERL_NIF_TERM nif_hash32(ErlNifEnv *env, int argc,
  const ERL_NIF_TERM argv[]);
static ERL_NIF_TERM nif_hash32_init(ErlNifEnv *env, int argc,
  const ERL_NIF_TERM argv[]);
static ERL_NIF_TERM nif_hash32_update(ErlNifEnv *env, int argc,
  const ERL_NIF_TERM argv[]);
static ERL_NIF_TERM nif_hash32_digest(ErlNifEnv *env, int argc,
  const ERL_NIF_TERM argv[]);
static ERL_NIF_TERM nif_hash64(ErlNifEnv *env, int argc,
  const ERL_NIF_TERM argv[]);
static ERL_NIF_TERM nif_hash64_init(ErlNifEnv *env, int argc,
  const ERL_NIF_TERM argv[]);
static ERL_NIF_TERM nif_hash64_update(ErlNifEnv *env, int argc,
  const ERL_NIF_TERM argv[]);
static ERL_NIF_TERM nif_hash64_digest(ErlNifEnv *env, int argc,
  const ERL_NIF_TERM argv[]);

static ERL_NIF_TERM atom_ok;

static ErlNifFunc nif_funcs[] = {
  {"hash32_impl", 2, nif_hash32, 0},
  {"hash32_init_impl", 1, nif_hash32_init, 0},
  {"hash32_update_impl", 2, nif_hash32_update, 0},
  {"hash32_digest_impl", 1, nif_hash32_digest, 0},
  {"hash64_impl", 2, nif_hash64, 0},
  {"hash64_init_impl", 1, nif_hash64_init, 0},
  {"hash64_update_impl", 2, nif_hash64_update, 0},
  {"hash64_digest_impl", 1, nif_hash64_digest, 0}
};

static ErlNifResourceType *xxhash_handle;


// Hash32.

static ERL_NIF_TERM
nif_hash32(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]) {
  ErlNifBinary src_bin;
  unsigned int hash;
  unsigned int seed;
  UNUSED(argc);

  if (!enif_inspect_binary(env, argv[0], &src_bin) ||
      !enif_get_uint(env, argv[1], &seed)) {
    return enif_make_badarg(env);
  }

  hash = XXH32(src_bin.data, src_bin.size, seed);

  return enif_make_uint(env, hash);
}


static ERL_NIF_TERM
nif_hash32_init(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]) {
  XXH32_state_t *state = enif_alloc_resource(xxhash_handle, sizeof(XXH32_state_t));
  unsigned int seed;
  ERL_NIF_TERM term;
  UNUSED(argc);
  if (!enif_get_uint(env, argv[0], &seed)) {
    return enif_make_badarg(env);
  }
  XXH32_reset(state, seed);
  term = enif_make_resource(env, state);
  enif_release_resource(state);
  return term;
}

static ERL_NIF_TERM
nif_hash32_update(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]) {
  XXH32_state_t *state;
  ErlNifBinary src_bin;
  UNUSED(argc);
  if (!enif_get_resource(env, argv[0], xxhash_handle, (void **) &state) ||
      !enif_inspect_binary(env, argv[1], &src_bin)) {
    return enif_make_badarg(env);
  }
  XXH32_update(state, src_bin.data, src_bin.size);
  return atom_ok;
}

static ERL_NIF_TERM
nif_hash32_digest(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]) {
  XXH32_state_t *state;
  unsigned int hash;
  UNUSED(argc);
  if (!enif_get_resource(env, argv[0], xxhash_handle, (void **) &state)) {
    return enif_make_badarg(env);
  }
  hash = XXH32_digest(state);
  return enif_make_uint(env, hash);
}


// Hash64.

static ERL_NIF_TERM
nif_hash64(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]) {
  ErlNifBinary src_bin;
  uint64_t hash;
  ErlNifUInt64 seed;
  UNUSED(argc);

  if (!enif_inspect_binary(env, argv[0], &src_bin) ||
      !enif_get_uint64(env, argv[1], (ErlNifUInt64 *) &seed)) {
    return enif_make_badarg(env);
  }

  hash = XXH64(src_bin.data, src_bin.size, seed);

  return enif_make_uint64(env, hash);
}


static ERL_NIF_TERM
nif_hash64_init(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]) {
  XXH64_state_t *state = enif_alloc_resource(xxhash_handle, sizeof(XXH64_state_t));
  ErlNifUInt64 seed;
  ERL_NIF_TERM term;
  UNUSED(argc);
  if (!enif_get_uint64(env, argv[0], &seed)) {
    return enif_make_badarg(env);
  }
  XXH64_reset(state, seed);
  term = enif_make_resource(env, state);
  enif_release_resource(state);
  return term;
}

static ERL_NIF_TERM
nif_hash64_update(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]) {
  XXH64_state_t *state;
  ErlNifBinary src_bin;
  UNUSED(argc);
  if (!enif_get_resource(env, argv[0], xxhash_handle, (void **) &state) ||
      !enif_inspect_binary(env, argv[1], &src_bin)) {
    return enif_make_badarg(env);
  }
  XXH64_update(state, src_bin.data, src_bin.size);
  return atom_ok;
}

static ERL_NIF_TERM
nif_hash64_digest(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]) {
  XXH64_state_t *state;
  uint64_t hash;
  UNUSED(argc);
  if (!enif_get_resource(env, argv[0], xxhash_handle, (void **) &state)) {
    return enif_make_badarg(env);
  }
  hash = XXH64_digest(state);
  return enif_make_uint64(env, hash);
}


// Loading and cleaning stuff.

static int init(ErlNifEnv *env, void* *priv_data, ERL_NIF_TERM load_info) {
  UNUSED(priv_data);
  UNUSED(load_info);
  atom_ok = enif_make_atom(env, "ok");
  if ((xxhash_handle = enif_open_resource_type(env, "xxhash", "xxhash_handle", &xxhash_cleanup,
        ERL_NIF_RT_CREATE | ERL_NIF_RT_TAKEOVER, NULL)) == NULL) {
    return -1;
  }
  return 0;
}

void xxhash_cleanup(ErlNifEnv *env, void *obj) {
  UNUSED(env);
  enif_release_resource(obj);
}


// Init stuff.

ERL_NIF_INIT(xxhash, nif_funcs, &init, NULL, NULL, NULL);
