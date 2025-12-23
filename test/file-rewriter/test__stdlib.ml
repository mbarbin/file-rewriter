(******************************************************************************)
(*  Copyright 2024 Mathieu Barbin <mathieu.barbin@gmail.com>                  *)
(*                                                                            *)
(*  Licensed under the Apache License, Version 2.0 (the "License");           *)
(*  you may not use this file except in compliance with the License.          *)
(*  You may obtain a copy of the License at                                   *)
(*                                                                            *)
(*  http://www.apache.org/licenses/LICENSE-2.0                                *)
(*                                                                            *)
(*  Unless required by applicable law or agreed to in writing, software       *)
(*  distributed under the License is distributed on an "AS IS" BASIS,         *)
(*  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.  *)
(*  See the License for the specific language governing permissions and       *)
(*  limitations under the License.                                            *)
(******************************************************************************)

let%expect_test "require" =
  require_does_raise (fun () -> require false);
  [%expect {| Failure("Required condition does not hold") |}];
  ()
;;

let%expect_test "require_does_raise did not raise" =
  (match require_does_raise ignore with
   | () -> assert false
   | exception exn -> print_string (Printexc.to_string exn));
  [%expect {| ("Did not raise.", {}) |}];
  ()
;;

let%expect_test "require_equal not equal" =
  (match require_equal (module String) "0" "42" with
   | () -> assert false
   | exception exn -> print_string (Printexc.to_string exn));
  [%expect {| ("Values are not equal.", { v1 = "0"; v2 = "42" }) |}];
  ()
;;
