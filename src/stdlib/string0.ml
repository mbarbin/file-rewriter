(*********************************************************************************)
(*  file-rewriter: Apply small rewrites to tweak or refactor your files          *)
(*  SPDX-FileCopyrightText: 2024-2026 Mathieu Barbin <mathieu.barbin@gmail.com>  *)
(*  SPDX-License-Identifier: Apache-2.0                                          *)
(*********************************************************************************)

include Stdlib.StringLabels

let compare a b = Ordering.of_int (compare a b)
let to_dyn = Dyn.string
let split t ~on = split_on_char ~sep:on t
