(*_********************************************************************************)
(*_  file-rewriter: Apply small rewrites to tweak or refactor your files          *)
(*_  SPDX-FileCopyrightText: 2024-2026 Mathieu Barbin <mathieu.barbin@gmail.com>  *)
(*_  SPDX-License-Identifier: Apache-2.0                                          *)
(*_********************************************************************************)

module Code_error = Code_error0
module Dyn = Dyn0
module List = List0
module Ordering = Ordering0
module String = String0
module With_equal_and_dyn = With_equal_and_dyn0

val print_dyn : Dyn.t -> unit
val phys_equal : 'a -> 'a -> bool
val require : bool -> unit
val require_does_raise : (unit -> 'a) -> unit
val require_equal : (module With_equal_and_dyn.S with type t = 'a) -> 'a -> 'a -> unit
