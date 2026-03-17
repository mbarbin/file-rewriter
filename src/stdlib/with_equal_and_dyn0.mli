(*_********************************************************************************)
(*_  file-rewriter: Apply small rewrites to tweak or refactor your files          *)
(*_  SPDX-FileCopyrightText: 2024-2026 Mathieu Barbin <mathieu.barbin@gmail.com>  *)
(*_  SPDX-License-Identifier: Apache-2.0                                          *)
(*_********************************************************************************)

module type S = sig
  type t

  val equal : t -> t -> bool
  val to_dyn : t -> Dyn.t
end
