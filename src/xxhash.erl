%%% @author Pierre Matri  <pierre@matri.me>
%%%
%%% @copyright 2013 Pierre Matri, All rights reserved. Open source, MIT License.
%%% @version 0.1.0

-module(xxhash).
-version(0.1).

-define(DEFAULT_SEED, 0).

-export([hash32/1, hash32/2,
         hash32_init/0, hash32_init/1,
         hash32_update/2, hash32_final/1]).

-on_load(init/0).

-spec init() -> ok | {error, any()}.

-define(nif_stub, nif_stub_error(?LINE)).
nif_stub_error(Line) ->
    erlang:nif_error({nif_not_loaded, module, ?MODULE, line, Line}).


init() ->
    PrivDir = case code:priv_dir(?MODULE) of
                  {error, bad_name} ->
                      EbinDir = filename:dirname(code:which(?MODULE)),
                      AppPath = filename:dirname(EbinDir),
                      filename:join(AppPath, "priv");
                  Path ->
                      Path
              end,

    erlang:load_nif(filename:join(PrivDir, xxhash_nif), 0).


%%%=============================================================================
%%% Nif functions
%%%=============================================================================

hash32_impl(_Data, _Seed) ->
  ?nif_stub.

hash32_init_impl(_Seed) ->
  ?nif_stub.

hash32_update_impl(_Context, _Data) ->
  ?nif_stub.

hash32_final_impl(_Context) ->
  ?nif_stub.


%%%=============================================================================
%%% Exports
%%%=============================================================================

hash32(Data) ->
  hash32(Data, ?DEFAULT_SEED).

hash32(Data, Seed) when is_integer(Seed) ->
  hash32_impl(supported_to_binary(Data), Seed).

hash32_init() ->
  hash32_init(?DEFAULT_SEED).

hash32_init(Seed) when is_integer(Seed) ->
  hash32_init_impl(Seed).

hash32_update(Context, Data) when is_binary(Context) ->
  hash32_update_impl(Context, supported_to_binary(Data)).


hash32_final(Context) when is_binary(Context) ->
  hash32_final_impl(Context).


%%%=============================================================================
%%% Helpers
%%%=============================================================================

supported_to_binary(Data) when is_binary(Data) ->
  Data;
supported_to_binary(Data) when is_list(Data) ->
  list_to_binary(Data);
supported_to_binary(Data) when is_atom(Data) ->
  term_to_binary(Data);
supported_to_binary(Data) when is_integer(Data) ->
  list_to_binary(integer_to_list(Data));
supported_to_binary(Data) when is_float(Data) ->
  <<Data/float>>.


%%%=============================================================================
%%% Tests
%%%=============================================================================

-ifdef(TEST).

-include_lib("eunit/include/eunit.hrl").

hash32_test() ->
  Hash1 = hash32(<<"FooBar">>),
  Handle = hash32_init(),
  ok = hash32_update(Handle, <<"Foo">>),
  ok = hash32_update(Handle, <<"Bar">>),
  Hash2 = hash32_final(Handle),
  ?assertEqual(Hash1, Hash2).

hash32_seed_test() ->
  Hash1 = hash32(<<"FooBar">>, 50),
  Handle = hash32_init(50),
  ok = hash32_update(Handle, <<"Foo">>),
  ok = hash32_update(Handle, <<"Bar">>),
  Hash2 = hash32_final(Handle),
  ?assertEqual(Hash1, Hash2).

hash32_seed2_test() ->
  Hash1 = hash32(<<"FooBar">>, 50),
  Hash2 = hash32(<<"FooBar">>),
  ?assertNot(Hash1 == Hash2).

hash32_list_test() ->
  Hash1 = hash32([1, 42]),
  Hash2 = hash32([1, 220]),
  ?assertNot(Hash1 == Hash2).

hash32_atom_test() ->
  Hash1 = hash32(foo),
  Hash2 = hash32(bar),
  ?assertNot(Hash1 == Hash2).

hash32_int_test() ->
  Hash1 = hash32(42),
  Hash2 = hash32(67),
  ?assertNot(Hash1 == Hash2).

hash32_float_test() ->
  Hash1 = hash32(5.98),
  Hash2 = hash32(5.97),
  ?assertNot(Hash1 == Hash2).

-endif.