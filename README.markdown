Erlang External Term Format for OCaml
=====================================

Provides all encoding and decoding for the Erlang External Term Format
(as defined at [http://erlang.org/doc/apps/erts/erl_ext_dist.html](http://erlang.org/doc/apps/erts/erl_ext_dist.html))
in a single OCaml module.

Requires `OCaml >= 4.04.0`

Build
-----

With dune:

    opam install zarith
    dune build

With local dependencies:

    make

Test
----

With dune:

    dune test

With local dependencies:

    ./tests

Author
------

Michael Truog (mjtruog at protonmail dot com)

License
-------

MIT License
