(*_********************************************************************************)
(*_  file-rewriter: Apply small rewrites to tweak or refactor your files          *)
(*_  SPDX-FileCopyrightText: 2024-2026 Mathieu Barbin <mathieu.barbin@gmail.com>  *)
(*_  SPDX-License-Identifier: Apache-2.0                                          *)
(*_********************************************************************************)

(** Extending [Stdlib] for use in the tests in this project. *)

include module type of struct
  include Stdlib0
end
