(*********************************************************************************)
(*  file-rewriter: Apply small rewrites to tweak or refactor your files          *)
(*  SPDX-FileCopyrightText: 2024-2026 Mathieu Barbin <mathieu.barbin@gmail.com>  *)
(*  SPDX-License-Identifier: Apache-2.0                                          *)
(*********************************************************************************)

type t =
  { message : string
  ; data : (string * Dyn.t) list
  }

exception E of t

let raise message data = raise (E { message; data })
let to_dyn { message; data } = Dyn.Tuple [ Dyn.String message; Record data ]

let () =
  Printexc.register_printer (function
    | E t -> Some (Dyn.to_string (to_dyn t))
    | _ -> None [@coverage off])
;;
