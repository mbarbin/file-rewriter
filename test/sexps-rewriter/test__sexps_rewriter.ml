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

(* We test the library by using it to apply rewrites to a dune file. *)

(* [Sexps_rewriter] doesn't need to access the disk, but we're saving files on
   disk because [Err] can show file parts when displaying errors if the location
   points to an existing file, which makes the tests nicer and more
   understandable. *)
let save_file ~path ~contents =
  let paths = String.split (Fpath.to_string path) ~on:'/' in
  let rec aux dir = function
    | [] -> (assert false [@coverage off])
    | [ _ ] -> ()
    | item :: (_ :: _ as tl) ->
      let dir = dir ^ "/" ^ item in
      Unix.mkdir dir 0o777;
      aux dir tl
  in
  aux "." paths;
  Out_channel.with_open_bin (Fpath.to_string path) (fun oc ->
    Out_channel.output_string oc contents)
;;

let original_contents =
  {|
; Hey this is a comment, which we should preserve !!!
(library
  (name sexps_rewriter_test)
  (flags -w +a-4-40-42-44-66 -warn-error +a)
  (libraries
   expect_test_helpers_core
   base
   sexps_rewriter
   file_rewriter
   parsexp)
  ; This is another comment.
  (inline_tests)
  (lint (pps ppx_js_style -check-doc-comments))
  (preprocess
   (pps ppx_sexp_conv ppx_sexp_value)))
|}
  |> String.strip
  |> fun s -> s ^ "\n"
;;

let%expect_test "libraries sorting" =
  Err.For_test.wrap
  @@ fun () ->
  let path = Fpath.v "path/lib/dune" in
  let sexps_rewriter =
    match Sexps_rewriter.create ~path ~original_contents with
    | Ok t -> t
    | Error { loc = _; message = _ } -> assert false
  in
  let () =
    let sexps, (_ : Parsexp.Positions.t) =
      Sexps_rewriter.Private.parser_result sexps_rewriter
    in
    (* How many sexps did we just parse here? *)
    print_s [%sexp { num_sexps = (List.length sexps : int) }];
    [%expect {| ((num_sexps 1)) |}];
    let original_sexps = Sexps_rewriter.original_sexps sexps_rewriter in
    print_s [%sexp { num_sexps = (List.length original_sexps : int) }];
    [%expect {| ((num_sexps 1)) |}];
    require [%here] (phys_equal original_sexps sexps);
    [%expect {||}]
  in
  print_endline (Sexps_rewriter.path sexps_rewriter |> Fpath.to_string);
  [%expect {| path/lib/dune |}];
  print_endline
    (Sexps_rewriter.file_rewriter sexps_rewriter |> File_rewriter.path |> Fpath.to_string);
  [%expect {| path/lib/dune |}];
  (* If we do nothing, the output shall be equal to that with which we started. *)
  require_equal
    [%here]
    (module String)
    original_contents
    (Sexps_rewriter.contents sexps_rewriter);
  [%expect {||}];
  let () =
    match Sexps_rewriter.contents_result sexps_rewriter with
    | Error _ -> assert false
    | Ok contents ->
      require_equal [%here] (module String) original_contents contents;
      [%expect {||}]
  in
  let print_diff () =
    let modified_contents = Sexps_rewriter.contents sexps_rewriter in
    Expect_test_patdiff.print_patdiff original_contents modified_contents ~context:3
  in
  print_diff ();
  [%expect {||}];
  (* Let's start with a small experiment: we will rename "name" to "public_name". *)
  Sexps_rewriter.visit sexps_rewriter ~f:(fun sexp ~range ~file_rewriter ->
    match sexp with
    | Atom "name" ->
      File_rewriter.replace file_rewriter ~range ~text:"public_name";
      Break
    | _ -> Continue);
  print_diff ();
  [%expect
    {|
    -1,6 +1,6
      ; Hey this is a comment, which we should preserve !!!
      (library
    -|  (name sexps_rewriter_test)
    +|  (public_name sexps_rewriter_test)
        (flags -w +a-4-40-42-44-66 -warn-error +a)
        (libraries
         expect_test_helpers_core
    |}];
  (* Here, we reorder the libraries alphabetically. We are doing that by adding
     a substitution for all of the library names, one by one. There might be
     simpler way to do this, but this is a good test for [file_rewriter]. *)
  Sexps_rewriter.visit sexps_rewriter ~f:(fun sexp ~range:_ ~file_rewriter ->
    match sexp with
    | List (Atom "libraries" :: libraries) ->
      let reordered =
        List.map libraries ~f:(function
          | Atom name -> name
          | List _ -> assert false)
        |> List.sort ~compare:String.compare
      in
      List.iter2_exn libraries reordered ~f:(fun sexp name ->
        File_rewriter.replace
          file_rewriter
          ~range:(Sexps_rewriter.range sexps_rewriter sexp)
          ~text:name);
      Break
    | _ -> Continue);
  print_diff ();
  [%expect
    {|
    -1,13 +1,13
      ; Hey this is a comment, which we should preserve !!!
      (library
    -|  (name sexps_rewriter_test)
    +|  (public_name sexps_rewriter_test)
        (flags -w +a-4-40-42-44-66 -warn-error +a)
        (libraries
    +|   base
         expect_test_helpers_core
    -|   base
    -|   sexps_rewriter
    -|   file_rewriter
    -|   parsexp)
    +|   file_rewriter
    +|   parsexp
    +|   sexps_rewriter)
        ; This is another comment.
        (inline_tests)
        (lint (pps ppx_js_style -check-doc-comments))
    |}];
  (* Let's show the end result. *)
  print_endline (Sexps_rewriter.contents sexps_rewriter);
  [%expect
    {|
    ; Hey this is a comment, which we should preserve !!!
    (library
      (public_name sexps_rewriter_test)
      (flags -w +a-4-40-42-44-66 -warn-error +a)
      (libraries
       base
       expect_test_helpers_core
       file_rewriter
       parsexp
       sexps_rewriter)
      ; This is another comment.
      (inline_tests)
      (lint (pps ppx_js_style -check-doc-comments))
      (preprocess
       (pps ppx_sexp_conv ppx_sexp_value)))
    |}];
  (* Now we're demonstrating another use case for this library, and that being
     an engine to produce linting errors and warnings.

     For the sake of the example here, let's assume we want to produce a warning
     if a certain library appears in the list of dependencies. *)
  save_file ~path ~contents:original_contents;
  Sexps_rewriter.visit sexps_rewriter ~f:(fun sexp ~range:_ ~file_rewriter:_ ->
    match sexp with
    | List (Atom "libraries" :: libraries) ->
      List.iter libraries ~f:(function
        | List _ -> assert false
        | Atom ("sexps_rewriter" as library_name) as sexp ->
          let loc = Sexps_rewriter.loc sexps_rewriter sexp in
          Err.warning
            ~loc
            [ Pp.textf "Hey, you're using the cool '%s' library!" library_name
            ; Pp.textf "How awesome is that?"
            ]
            ~hints:[ Pp.verbatim "Keep up the good work." ]
        | Atom _ -> ());
      Skip
    | _ -> Continue);
  ();
  [%expect
    {|
    File "path/lib/dune", line 8, characters 3-17:
    8 |    sexps_rewriter
           ^^^^^^^^^^^^^^
    Warning: Hey, you're using the cool 'sexps_rewriter' library!
    How awesome is that?
    Hint: Keep up the good work.
    |}];
  ()
;;

(* We test here the behavior when the input contains an invalid sexp. *)
let syntax_error =
  {|
; Hey this is a comment, which we should preserve !!!
(library
  (name sexps_rewriter_test)
  (flags -w +a-4-40-42-44-66 -warn-error +a)
  (libraries
   core
   expect_test_helpers_core
   sexps_rewriter
   file_rewriter
   parsexp)
  ; This is another comment.
  (inline_tests
  (preprocess
   (pps ppx_jane ppx_js_style -check-doc-comments)))
|}
  |> String.strip
;;

let%expect_test "syntax-error" =
  Err.For_test.wrap
  @@ fun () ->
  let path = Fpath.v "my-dune-file" in
  save_file ~path ~contents:syntax_error;
  Err.For_test.protect (fun () ->
    match Sexps_rewriter.create ~path ~original_contents:syntax_error with
    | Ok (_ : Sexps_rewriter.t) -> assert false
    | Error { loc; message } -> Err.raise ~loc [ Pp.textf "%s" message ]);
  [%expect
    {|
    File "my-dune-file", line 14, characters 52-52:
    14 |    (pps ppx_jane ppx_js_style -check-doc-comments)))

    Error: unclosed parentheses at end of input
    [123]
    |}];
  ()
;;

let%expect_test "invalid positions" =
  Err.For_test.wrap
  @@ fun () ->
  let path = Fpath.v "path/lib/hello-world" in
  let original_contents = "(Hello World)" in
  let sexps_rewriter =
    match Sexps_rewriter.create ~path ~original_contents with
    | Ok t -> t
    | Error { loc = _; message = _ } -> assert false
  in
  require_does_raise [%here] (fun () ->
    Sexps_rewriter.position sexps_rewriter (List [ Atom "foo" ]));
  [%expect
    {|
    (Sexps_rewriter.Position_not_found
      (((line 1) (col 0)  (offset 0))
       ((line 1) (col 1)  (offset 1))
       ((line 1) (col 5)  (offset 5))
       ((line 1) (col 7)  (offset 7))
       ((line 1) (col 11) (offset 11))
       ((line 1) (col 12) (offset 12)))
      (foo))
    |}];
  ()
;;

let%expect_test "empty" =
  Err.For_test.wrap
  @@ fun () ->
  let path = Fpath.v "path/lib/empty" in
  let original_contents = "" in
  let sexps_rewriter =
    match Sexps_rewriter.create ~path ~original_contents with
    | Ok t -> t
    | Error { loc = _; message = _ } -> assert false
  in
  Sexps_rewriter.visit sexps_rewriter ~f:(fun _ ~range:_ ~file_rewriter:_ ->
    (assert false [@coverage off]));
  print_endline (Sexps_rewriter.contents sexps_rewriter);
  [%expect {||}];
  ()
;;

let%expect_test "atom" =
  Err.For_test.wrap
  @@ fun () ->
  let path = Fpath.v "path/lib/atom" in
  let original_contents = "Hello" in
  let sexps_rewriter =
    match Sexps_rewriter.create ~path ~original_contents with
    | Ok t -> t
    | Error { loc = _; message = _ } -> assert false
  in
  Sexps_rewriter.visit sexps_rewriter ~f:(fun sexp ~range:_ ~file_rewriter:_ ->
    match sexp with
    | Atom _ ->
      Err.warning
        ~loc:(Sexps_rewriter.loc sexps_rewriter sexp)
        [ Pp.text "This is an atom." ];
      Break
    | _ -> assert false);
  print_endline (Sexps_rewriter.contents sexps_rewriter);
  [%expect
    {|
    Hello
    File "path/lib/atom", line 1, characters 0-5:
    Warning: This is an atom.
    |}];
  ()
;;

let%expect_test "insert" =
  Err.For_test.wrap
  @@ fun () ->
  let path = Fpath.v "path/lib/hello-world" in
  let original_contents = "(Hello World)" in
  let sexps_rewriter =
    match Sexps_rewriter.create ~path ~original_contents with
    | Ok t -> t
    | Error { loc = _; message = _ } -> assert false
  in
  Sexps_rewriter.visit sexps_rewriter ~f:(fun sexp ~range:_ ~file_rewriter ->
    match sexp with
    | Atom "World" ->
      File_rewriter.insert
        file_rewriter
        ~offset:(Sexps_rewriter.start_offset sexps_rewriter sexp)
        ~text:"Big ";
      File_rewriter.insert
        file_rewriter
        ~offset:(Sexps_rewriter.stop_offset sexps_rewriter sexp)
        ~text:" !!";
      Continue
    | _ -> Continue);
  print_endline (Sexps_rewriter.contents sexps_rewriter);
  [%expect {| (Hello Big World !!) |}];
  ()
;;

let%expect_test "reset" =
  Err.For_test.wrap
  @@ fun () ->
  let path = Fpath.v "path/lib/atom" in
  let original_contents = "Hello World" in
  let sexps_rewriter =
    match Sexps_rewriter.create ~path ~original_contents with
    | Ok t -> t
    | Error { loc = _; message = _ } -> assert false
  in
  (* Here we check that we can call [visit] and [contents] multiple times, as
     long as rewrites are valid. *)
  Sexps_rewriter.visit sexps_rewriter ~f:(fun sexp ~range ~file_rewriter ->
    match sexp with
    | Atom "World" ->
      File_rewriter.replace file_rewriter ~range ~text:"You";
      Break
    | _ -> Continue);
  print_endline (Sexps_rewriter.contents sexps_rewriter);
  [%expect {| Hello You |}];
  Sexps_rewriter.visit sexps_rewriter ~f:(fun sexp ~range ~file_rewriter ->
    match sexp with
    | Atom "Hello" ->
      File_rewriter.replace file_rewriter ~range ~text:"Hi";
      Continue
    | _ -> Continue);
  print_endline (Sexps_rewriter.contents sexps_rewriter);
  [%expect {| Hi You |}];
  (* Checking that we can [reset] the rewrite and start fresh. *)
  Sexps_rewriter.reset sexps_rewriter;
  print_endline (Sexps_rewriter.contents sexps_rewriter);
  [%expect {| Hello World |}];
  Sexps_rewriter.visit sexps_rewriter ~f:(fun sexp ~range ~file_rewriter ->
    match sexp with
    | Atom "World" ->
      File_rewriter.replace file_rewriter ~range ~text:"Me";
      Break
    | _ -> Continue);
  print_endline (Sexps_rewriter.contents sexps_rewriter);
  [%expect {| Hello Me |}];
  ()
;;
