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

let path = Fpath.v "foo.txt"

let original_contents =
  {|
Hello World
Hello Newline
|}
  |> String.strip
;;

let%expect_test "insert" =
  let file_rewriter = File_rewriter.create ~path:(Fpath.v "foo.txt") ~original_contents in
  print_endline (File_rewriter.path file_rewriter |> Fpath.to_string);
  [%expect {| foo.txt |}];
  let reset () = File_rewriter.reset file_rewriter in
  let test () = print_endline (File_rewriter.contents file_rewriter) in
  (* The original contents may be accessed. *)
  require_equal
    [%here]
    (module String)
    original_contents
    (File_rewriter.original_contents file_rewriter);
  (* Applying no substitution shall return the original contents. *)
  require_equal
    [%here]
    (module String)
    original_contents
    (File_rewriter.contents file_rewriter);
  [%expect {||}];
  test ();
  [%expect
    {|
    Hello World
    Hello Newline |}];
  (* Inserting at invalid offsets raises [Invalid_argument]. *)
  require_does_raise [%here] (fun () ->
    File_rewriter.insert file_rewriter ~offset:(-1) ~text:"Prefix\n");
  [%expect {| (Invalid_argument File_rewriter.insert) |}];
  require_does_raise [%here] (fun () ->
    File_rewriter.insert
      file_rewriter
      ~offset:(String.length original_contents + 1)
      ~text:"Prefix\n");
  [%expect {| (Invalid_argument File_rewriter.insert) |}];
  (* We shall be able to apply a subst from the very beginning. *)
  File_rewriter.insert file_rewriter ~offset:0 ~text:"Prefix\n";
  test ();
  [%expect
    {|
    Prefix
    Hello World
    Hello Newline |}];
  (* But we don't have to, in which case the first part shall not be truncated. *)
  reset ();
  File_rewriter.insert file_rewriter ~offset:6 ~text:"Friendly ";
  test ();
  [%expect
    {|
    Hello Friendly World
    Hello Newline |}];
  (* It is supported to insert several texts at the same offset. When doing so,
     the order of insertions shall be respected, and insertion requested first
     are inserted first in left-to-right order. *)
  File_rewriter.insert file_rewriter ~offset:6 ~text:"Awesome ";
  test ();
  [%expect
    {|
    Hello Friendly Awesome World
    Hello Newline
    |}];
  File_rewriter.insert
    file_rewriter
    ~offset:(String.length "Hello World\n")
    ~text:"Inserting a newline\n";
  test ();
  [%expect
    {|
    Hello Friendly Awesome World
    Inserting a newline
    Hello Newline
    |}];
  (* Inserting at the very end shall be supported too. *)
  File_rewriter.insert file_rewriter ~offset:(String.length original_contents) ~text:"...";
  test ();
  [%expect
    {|
    Hello Friendly Awesome World
    Inserting a newline
    Hello Newline...
    |}];
  ()
;;

let%expect_test "replace" =
  let file_rewriter = File_rewriter.create ~path ~original_contents in
  let reset () = File_rewriter.reset file_rewriter in
  let test () = print_endline (File_rewriter.contents file_rewriter) in
  File_rewriter.remove
    file_rewriter
    ~range:{ start = String.length "Hello"; stop = String.length "Hello World" };
  test ();
  [%expect
    {|
    Hello
    Hello Newline
    |}];
  reset ();
  File_rewriter.replace
    file_rewriter
    ~range:{ start = String.length "Hello "; stop = String.length "Hello World" }
    ~text:"You";
  test ();
  [%expect
    {|
    Hello You
    Hello Newline
    |}];
  (* The order between insertions and replaces shall not matter, the insertions
     are going to be placed first. *)
  File_rewriter.insert file_rewriter ~offset:6 ~text:"Awesome ";
  test ();
  [%expect
    {|
    Hello Awesome You
    Hello Newline
    |}];
  reset ();
  (* Indeed, here we show that applying the insert first does yield the same result. *)
  File_rewriter.insert file_rewriter ~offset:6 ~text:"Awesome ";
  File_rewriter.replace
    file_rewriter
    ~range:{ start = String.length "Hello "; stop = String.length "Hello World" }
    ~text:"You";
  test ();
  [%expect
    {|
    Hello Awesome You
    Hello Newline
    |}];
  reset ();
  (* However, if you try inserting a text into a part that was removed, this is
     not going to work. *)
  File_rewriter.replace
    file_rewriter
    ~range:{ start = 0; stop = String.length "Hello World" }
    ~text:"Hi, You!";
  test ();
  [%expect
    {|
    Hi, You!
    Hello Newline
    |}];
  require_does_raise [%here] (fun () ->
    File_rewriter.insert file_rewriter ~offset:(String.length "Hello ") ~text:"Awesome ";
    test ());
  [%expect
    {|
    (File_rewriter.Invalid_rewrites foo.txt
      ((start 0) (stop 11) (replace_by "Hi, You!"))
      ((start 6) (stop 6)  (replace_by "Awesome ")))
    |}];
  reset ();
  ()
;;

let%expect_test "invalid replaces" =
  let file_rewriter = File_rewriter.create ~path:(Fpath.v "foo.txt") ~original_contents in
  let out_of_bound = String.length original_contents + 1 in
  (* Invalid replace requests raises [Invalid_argument]. *)
  require_does_raise [%here] (fun () ->
    File_rewriter.replace file_rewriter ~range:{ start = -1; stop = 0 } ~text:"Text");
  [%expect {| (Invalid_argument File_rewriter.replace) |}];
  require_does_raise [%here] (fun () ->
    File_rewriter.replace
      file_rewriter
      ~range:{ start = out_of_bound; stop = out_of_bound + 1 }
      ~text:"Text");
  [%expect {| (Invalid_argument File_rewriter.replace) |}];
  require_does_raise [%here] (fun () ->
    File_rewriter.replace file_rewriter ~range:{ start = 0; stop = -1 } ~text:"Text");
  [%expect {| (Invalid_argument File_rewriter.replace) |}];
  require_does_raise [%here] (fun () ->
    File_rewriter.replace
      file_rewriter
      ~range:{ start = 0; stop = out_of_bound }
      ~text:"Text");
  [%expect {| (Invalid_argument File_rewriter.replace) |}];
  require_does_raise [%here] (fun () ->
    File_rewriter.replace file_rewriter ~range:{ start = 1; stop = 0 } ~text:"Text");
  [%expect {| (Invalid_argument File_rewriter.replace) |}];
  ()
;;

let%expect_test "invalid rewrites" =
  let file_rewriter = File_rewriter.create ~path ~original_contents in
  let reset () = File_rewriter.reset file_rewriter in
  let test () = print_endline (File_rewriter.contents file_rewriter) in
  reset ();
  (* The order in which [replace] request are performed doesn't matter, as long
     as the rewrites do not overlap. For example, the following is valid: *)
  File_rewriter.replace
    file_rewriter
    ~range:{ start = 2; stop = String.length "Hello World" }
    ~text:"You!";
  File_rewriter.replace file_rewriter ~range:{ start = 0; stop = 2 } ~text:"Hi, ";
  test ();
  [%expect
    {|
    Hi, You!
    Hello Newline
    |}];
  reset ();
  (* Rejected cases include:

     - 1. Inserting a text within a part that was rewritten or removed.
     - 2. Consecutive rewrites with overlapping offsets.
  *)
  (* 1. Inserting a text within a part that was rewritten or removed. *)
  File_rewriter.replace
    file_rewriter
    ~range:{ start = 0; stop = String.length "Hello" }
    ~text:"Hi";
  test ();
  [%expect
    {|
    Hi World
    Hello Newline
    |}];
  (* This may be counter intuitive, but the offset points to character in the
     original text, not the text being rewritten. For example, the following
     insertion request is invalid, because after the first replace that was
     performed, the current offset to the original text is at char 5, thus
     inserting at char 3 fails. *)
  File_rewriter.insert file_rewriter ~offset:3 ~text:"Big ";
  require_does_raise [%here] (fun () -> test ());
  [%expect
    {|
    (File_rewriter.Invalid_rewrites foo.txt
      ((start 0) (stop 5) (replace_by Hi))
      ((start 3) (stop 3) (replace_by "Big ")))
    |}];
  reset ();
  (* Here is a suitable solution for what was attempted above: *)
  File_rewriter.replace
    file_rewriter
    ~range:{ start = 0; stop = String.length "Hello" }
    ~text:"Hi";
  File_rewriter.insert file_rewriter ~offset:(String.length "Hello ") ~text:"Big ";
  test ();
  [%expect
    {|
    Hi Big World
    Hello Newline
    |}];
  (* 2. Consecutive rewrites with overlapping offsets. *)
  reset ();
  File_rewriter.replace
    file_rewriter
    ~range:{ start = 0; stop = String.length "Hello" }
    ~text:"Hi";
  test ();
  [%expect
    {|
    Hi World
    Hello Newline
    |}];
  (* Similarly to what we saw with an invalid offset on insertion, the following
     doesn't work since the ongoing buffer is already pointing at character 5,
     and thus position 3 is overlapping. *)
  File_rewriter.replace
    file_rewriter
    ~range:{ start = String.length "Hi "; stop = String.length "Hi World" }
    ~text:"Universe";
  require_does_raise [%here] (fun () -> test ());
  [%expect
    {|
    (File_rewriter.Invalid_rewrites foo.txt
      ((start 0) (stop 5) (replace_by Hi))
      ((start 3) (stop 8) (replace_by Universe)))
    |}];
  (* More coverage cases with 3 rewrites. *)
  reset ();
  File_rewriter.replace file_rewriter ~range:{ start = 0; stop = 2 } ~text:"Hi";
  File_rewriter.replace file_rewriter ~range:{ start = 1; stop = 3 } ~text:"Hi";
  File_rewriter.replace file_rewriter ~range:{ start = 3; stop = 5 } ~text:"Hi";
  require_does_raise [%here] (fun () -> test ());
  [%expect
    {|
    (File_rewriter.Invalid_rewrites foo.txt
      ((start 0) (stop 2) (replace_by Hi))
      ((start 1) (stop 3) (replace_by Hi)))
    |}];
  ()
;;

let%expect_test "contents_result" =
  let file_rewriter = File_rewriter.create ~path ~original_contents in
  let reset () = File_rewriter.reset file_rewriter in
  reset ();
  (* The order in which [replace] request are performed doesn't matter, as long
     as the rewrites do not overlap. For example, the following is valid: *)
  File_rewriter.replace
    file_rewriter
    ~range:{ start = 2; stop = String.length "Hello World" }
    ~text:"You!";
  File_rewriter.replace file_rewriter ~range:{ start = 0; stop = 2 } ~text:"Hi, ";
  let () =
    match File_rewriter.contents_result file_rewriter with
    | Ok contents -> print_endline contents
    | Error _ -> assert false
  in
  [%expect
    {|
    Hi, You!
    Hello Newline
    |}];
  File_rewriter.insert file_rewriter ~offset:3 ~text:"Big ";
  let () =
    match File_rewriter.contents_result file_rewriter with
    | Ok (_ : string) -> assert false
    | Error invalid_rewrites ->
      print_endline
        (Sexp.to_string_hum
           (invalid_rewrites |> File_rewriter.Invalid_rewrites.sexp_of_t));
      print_dyn (invalid_rewrites |> File_rewriter.Invalid_rewrites.to_dyn);
      ()
  in
  [%expect
    {|
    (foo.txt ((start 2) (stop 11) (replace_by You!))
     ((start 3) (stop 3) (replace_by "Big ")))
    ("foo.txt",
     { start = 2; stop = 11; replace_by = "You!" },
     { start = 3; stop = 3; replace_by = "Big " })
    |}];
  ()
;;
