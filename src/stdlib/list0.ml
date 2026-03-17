(*********************************************************************************)
(*  file-rewriter: Apply small rewrites to tweak or refactor your files          *)
(*  SPDX-FileCopyrightText: 2024-2026 Mathieu Barbin <mathieu.barbin@gmail.com>  *)
(*  SPDX-License-Identifier: Apache-2.0                                          *)
(*********************************************************************************)

include Stdlib.ListLabels

let iter t ~f = iter ~f t
let map t ~f = map ~f t
let sort t ~compare = sort ~cmp:compare t
