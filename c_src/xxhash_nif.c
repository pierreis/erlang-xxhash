#include "erl_nif.h"
#include "xxhash.h"

#define UNUSED(x) (void)(x)


// Misc. definitions

void xxhash_cleanup(ErlNifEnv *env, void *p);

typedef struct handle {
  void *instance;
} handle_t;


// Erlang terms and exports.

static ERL_NIF_TERM nif_hash32(ErlNifEnv *env, int argc,
  const ERL_NIF_TERM argv[]);
static ERL_NIF_TERM nif_hash32_init(ErlNifEnv *env, int argc,
  const ERL_NIF_TERM argv[]);
static ERL_NIF_TERM nif_hash32_update(ErlNifEnv *env, int argc,
  const ERL_NIF_TERM argv[]);
static ERL_NIF_TERM nif_hash32_digest(ErlNifEnv *env, int argc,
  const ERL_NIF_TERM argv[]);
static ERL_NIF_TERM nif_hash32_final(ErlNifEnv *env, int argc,
  const ERL_NIF_TERM argv[]);

static ERL_NIF_TERM atom_ok;

static ErlNifFunc nif_funcs[] = {
  {"hash32_impl", 2, nif_hash32},
  {"hash32_init_impl", 1, nif_hash32_init},
  {"hash32_update_impl", 2, nif_hash32_update},
  {"hash32_digest_impl", 1, nif_hash32_digest},
  {"hash32_final_impl", 1, nif_hash32_final}
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
  handle_t *handle = enif_alloc_resource(xxhash_handle, sizeof(handle_t));
  unsigned int seed;
  ERL_NIF_TERM term;
  UNUSED(argc);

  if (!enif_get_uint(env, argv[0], &seed)) {
    return enif_make_badarg(env);
  }

  handle->instance = XXH32_init(seed);

  term = enif_make_resource(env, handle);

  return term;
}

static ERL_NIF_TERM
nif_hash32_update(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]) {
  handle_t *handle;
  ErlNifBinary src_bin;
  UNUSED(argc);

  if (!enif_get_resource(env, argv[0], xxhash_handle, (void **) &handle) ||
      !enif_inspect_binary(env, argv[1], &src_bin)) {
    return enif_make_badarg(env);
  }

  XXH32_update(handle->instance, src_bin.data, src_bin.size);

  return atom_ok;
}

static ERL_NIF_TERM
nif_hash32_digest(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]) {
  handle_t* handle;
  unsigned int hash;
  UNUSED(argc);

  if (!enif_get_resource(env, argv[0], xxhash_handle, (void **) &handle)) {
    return enif_make_badarg(env);
  }

  hash = XXH32_intermediateDigest(handle->instance);

  return enif_make_uint(env, hash);
}

static ERL_NIF_TERM
nif_hash32_final(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]) {
  handle_t* handle;
  unsigned int hash;
  UNUSED(argc);

  if (!enif_get_resource(env, argv[0], xxhash_handle, (void **) &handle)) {
    return enif_make_badarg(env);
  }

  hash = XXH32_digest(handle->instance);

  return enif_make_uint(env, hash);
}


// Loading and cleaning stuff.

static int init(ErlNifEnv *env, void* *priv_data, ERL_NIF_TERM load_info) {
  UNUSED(priv_data);
  UNUSED(load_info);
  atom_ok = enif_make_atom(env, "ok");

  if ((xxhash_handle = enif_open_resource_type(env, "xxhash",
        "xxhash_handle", &xxhash_cleanup,
        ERL_NIF_RT_CREATE | ERL_NIF_RT_TAKEOVER, NULL)) == NULL) {
    return -1;
  }

  return 0;
}

void xxhash_cleanup(ErlNifEnv *env, void *obj) {
  handle_t *handle;
  UNUSED(env);

  handle = (handle_t *) obj;

  XXH32_digest(handle->instance);
  enif_release_resource(handle);
}


// Init stuff.

ERL_NIF_INIT(xxhash, nif_funcs, &init, NULL, NULL, NULL);