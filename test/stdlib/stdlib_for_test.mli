(*_*****************************************************************************)
(*_  Copyright 2024 Mathieu Barbin <mathieu.barbin@gmail.com>                  *)
(*_                                                                            *)
(*_  Licensed under the Apache License, Version 2.0 (the "License");           *)
(*_  you may not use this file except in compliance with the License.          *)
(*_  You may obtain a copy of the License at                                   *)
(*_                                                                            *)
(*_  http://www.apache.org/licenses/LICENSE-2.0                                *)
(*_                                                                            *)
(*_  Unless required by applicable law or agreed to in writing, software       *)
(*_  distributed under the License is distributed on an "AS IS" BASIS,         *)
(*_  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.  *)
(*_  See the License for the specific language governing permissions and       *)
(*_  limitations under the License.                                            *)
(*_*****************************************************************************)

(** Extending [Stdlib] for use in the tests in this project. *)

module Code_error = Code_error
module Dyn = Dyn

val print_dyn : Dyn.t -> unit
val phys_equal : 'a -> 'a -> bool

module List : sig
  include module type of struct
    include Stdlib.ListLabels
  end

  val iter : 'a t -> f:('a -> unit) -> unit
  val map : 'a t -> f:('a -> 'b) -> 'b t
  val sort : 'a t -> compare:('a -> 'a -> int) -> 'a t
end

module String : sig
  include module type of struct
    include Stdlib.StringLabels
  end

  val to_dyn : t -> Dyn.t
  val split : t -> on:char -> t list
end

val require : bool -> unit
val require_does_raise : (unit -> 'a) -> unit

module With_equal_and_dyn : sig
  module type S = sig
    type t

    val equal : t -> t -> bool
    val to_dyn : t -> Dyn.t
  end
end

val require_equal : (module With_equal_and_dyn.S with type t = 'a) -> 'a -> 'a -> unit
