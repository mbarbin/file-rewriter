(*********************************************************************************)
(*  file-rewriter: Apply small rewrites to tweak or refactor your files          *)
(*  SPDX-FileCopyrightText: 2024-2026 Mathieu Barbin <mathieu.barbin@gmail.com>  *)
(*  SPDX-License-Identifier: Apache-2.0                                          *)
(*********************************************************************************)

(* This is to silence `dune build @unused-libs` and keeping intended deps. *)
open! Stdlib_for_test

let%expect_test "empty" =
  ();
  [%expect {||}];
  ()
;;
