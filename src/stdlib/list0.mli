(*_********************************************************************************)
(*_  file-rewriter: Apply small rewrites to tweak or refactor your files          *)
(*_  SPDX-FileCopyrightText: 2024-2026 Mathieu Barbin <mathieu.barbin@gmail.com>  *)
(*_  SPDX-License-Identifier: Apache-2.0                                          *)
(*_********************************************************************************)

include module type of struct
  include Stdlib.ListLabels
end

val iter : 'a t -> f:('a -> unit) -> unit
val map : 'a t -> f:('a -> 'b) -> 'b t
val sort : 'a t -> compare:('a -> 'a -> int) -> 'a t
