erlang-xxhash [![Build Status](https://travis-ci.org/pierresforge/erlang-xxhash.png)](https://travis-ci.org/pierresforge/erlang-xxhash)
=============

Simple Erlang wrapper for the [xxHash](http://code.google.com/p/xxhash/) algorithm.


Usage
-----

```erlang
application:start(xxhash).
Text = "test".
Seed = 12345.


%% Simple usage

xxhash:hash32(Text, Seed).               % => 3834992036
xxhash:hash32(Text).                     % => 1042293711

xxhash:hash64(Text, Seed).               % => 7624679986283906467
xxhash:hash32(Text).                     % => 5754696928334414137


%% Advanced usage

Handle = xxhash:hash32_init(Seed).
xxhash:hash32_update(Handle, Text).      % => ok
xxhash:hash32_digest(Handle).            % => 3834992036 (Intermediate digest)
xxhash:hash32_update(Handle, <<"Foo">>). % Support for binary values.
xxhash:hash32_update(Handle, 42).        % Support for integers.
xxhash:hash32_update(Handle, 13.37).     % Support for floats.
xxhash:hash32_update(Handle, moo).       % Support for atoms.
xxhash:hash32_digest(Handle).            % => 3243777239

Handle = xxhash:hash64_init(Seed).
xxhash:hash64_update(Handle, Text).      % => ok
xxhash:hash64_digest(Handle).            % => 3834992036 (Intermediate digest)
xxhash:hash64_update(Handle, <<"Foo">>). % Support for binary values.
xxhash:hash64_update(Handle, 42).        % Support for integers.
xxhash:hash64_update(Handle, 13.37).     % Support for floats.
xxhash:hash64_update(Handle, moo).       % Support for atoms.
xxhash:hash64_digest(Handle).            % => 3243777239
```


Licenses
--------

This program is distributed under the MIT License.

xxHash library is distributed under New BSD License.


Author
------

Pierre Matri <pierre@matri.me>
