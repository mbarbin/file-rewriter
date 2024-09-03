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
   points to an existing file, which makes the test nicer and more
   understandable. *)
let save_file ~path ~contents =
  let paths = String.split (Fpath.to_string path) ~on:'/' in
  let rec aux dir = function
    | [] | [ _ ] -> ()
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
;;

let%expect_test "libraries sorting" =
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
    [%expect {| ((num_sexps 1)) |}]
  in
  (* If we do nothing, the output shall be equal to that with which we started. *)
  require_equal
    [%here]
    (module String)
    original_contents
    (Sexps_rewriter.contents sexps_rewriter);
  [%expect {||}];
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
        | Atom ("sexps_rewriter" as library_name) as sexp ->
          let loc = Sexps_rewriter.loc sexps_rewriter sexp in
          Err.warning
            ~loc
            [ Pp.textf "Hey, you're using the cool '%s' library!" library_name
            ; Pp.textf "How awesome is that?"
            ]
            ~hints:[ Pp.verbatim "Keep up the good work." ]
        | Atom _ | List _ -> ());
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
