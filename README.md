[![Build Status](https://travis-ci.org/p2pcollab/ocaml-ringcast.svg?branch=master)](https://travis-ci.org/p2pcollab/ocaml-ringcast)

# RingCast: P2P hybrid dissemination protocol

This is an OCaml implementation of RingCast,
a P2P hybrid (probabilistic/deterministic) dissemination protocol
described in the paper [PolderCast](https://hal.inria.fr/hal-01555561)
(and earlier in [Hybrid Dissemination](https://www.distributed-systems.net/my-data/papers/2007.mw.pdf)).

It organizes nodes in a bidirectional ring structure
and forwards messages to neighbours as well as random nodes.
It achieves complete dissemination of messages with a low message overhead.

This implementation is distributed under the MPL-2.0 license.

## Installation

``ringcast`` can be installed via `opam`:

    opam install ringcast
    opam install ringcast-lwt

## Building

To build from source, generate documentation, and run tests, use `dune`:

    dune build
    dune build @doc
    dune runtest -f -j1 --no-buffer

In addition, the following `Makefile` targets are available
 as a shorthand for the above:

    make all
    make build
    make doc
    make test

## Documentation

The documentation and API reference is generated from the source interfaces.
It can be consulted [online][doc] or via `odig`:

    odig doc ringcast
    odig doc ringcast-lwt

[doc]: https://p2pcollab.github.io/doc/ocaml-ringcast/
