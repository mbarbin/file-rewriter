# file-rewriter

[![CI Status](https://github.com/mbarbin/file-rewriter/workflows/ci/badge.svg)](https://github.com/mbarbin/file-rewriter/actions/workflows/ci.yml)
[![Coverage Status](https://coveralls.io/repos/github/mbarbin/file-rewriter/badge.svg?branch=main)](https://coveralls.io/github/mbarbin/file-rewriter?branch=main)

File-rewriter is an OCaml library for applying small rewrites to tweak or refactor your files. It provides a convenient interface to apply surgical textual substitutions on the fly, while navigating the contents of a file through an abstract representation containing code locations.

## Other libraries

The repository also contain `sexps_rewriter`, a specialized version of the main library dedicated to rewriting sexp files.

## Motivation

We created these libraries as part of an ongoing work to create linting and refactoring tools for the many `dune` files found in big monorepos (see [dunolint](https://github.com/mbarbin/dunolint)).

## Acknowledgments

We've been inspired by this [blog post](https://blog.janestreet.com/converting-a-code-base-from-camlp4-to-ppx/).

In particular, we've reused the idea of registering substitutions while itering through the locations of an AST. While we didn't reuse any particular project or existing code for our libraries, we're thankful to Jeremie Dimino and Jane Street for the work and technics discussed in their blog post.
