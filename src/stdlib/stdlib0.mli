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

module Code_error = Code_error0
module Dyn = Dyn0
module List = List0
module String = String0
module With_equal_and_dyn = With_equal_and_dyn0

val print_dyn : Dyn.t -> unit
val phys_equal : 'a -> 'a -> bool
val require : bool -> unit
val require_does_raise : (unit -> 'a) -> unit
val require_equal : (module With_equal_and_dyn.S with type t = 'a) -> 'a -> 'a -> unit
