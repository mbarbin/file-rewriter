(*_********************************************************************************)
(*_  file-rewriter: Apply small rewrites to tweak or refactor your files          *)
(*_  SPDX-FileCopyrightText: 2024-2026 Mathieu Barbin <mathieu.barbin@gmail.com>  *)
(*_  SPDX-License-Identifier: Apache-2.0                                          *)
(*_********************************************************************************)

include module type of struct
  include Stdlib.StringLabels
end

val compare : t -> t -> Ordering0.t
val to_dyn : t -> Dyn.t
val split : t -> on:char -> t list
