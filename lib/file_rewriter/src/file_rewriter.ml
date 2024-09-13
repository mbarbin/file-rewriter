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

type offset = Loc.Offset.t

type range = Loc.Range.t =
  { start : offset
  ; stop : offset
  }

module Rewrite = struct
  type t =
    { start : int
    ; stop : int
    ; replace_by : string
    }

  let sexp_of_t { start; stop; replace_by } =
    Sexplib0.Sexp.List
      [ List [ Atom "start"; Sexplib0.Sexp_conv.sexp_of_int start ]
      ; List [ Atom "stop"; Sexplib0.Sexp_conv.sexp_of_int stop ]
      ; List [ Atom "replace_by"; Atom replace_by ]
      ]
  ;;

  let compare (t1 : t) (t2 : t) =
    let r = Int.compare t1.start t2.start in
    if r <> 0 then r else Int.compare t1.stop t2.stop
  ;;
end

type t =
  { path : Fpath.t
  ; original_contents : string
  ; mutable rewrites : Rewrite.t list (* New elements added to the front. *)
  }

let path t = t.path
let original_contents t = t.original_contents
let create ~path ~original_contents = { path; original_contents; rewrites = [] }
let reset ({ path = _; original_contents = _; rewrites = _ } as t) = t.rewrites <- []

let insert t ~offset ~text =
  let length = String.length t.original_contents in
  if offset < 0 || offset > length then raise (Invalid_argument "File_rewriter.insert");
  t.rewrites <- { Rewrite.start = offset; stop = offset; replace_by = text } :: t.rewrites
;;

let replace t ~range:{ start; stop } ~text =
  let length = String.length t.original_contents in
  if start < 0 || start > length || stop < 0 || stop > length || start > stop
  then raise (Invalid_argument "File_rewriter.replace");
  t.rewrites <- { Rewrite.start; stop; replace_by = text } :: t.rewrites
;;

let remove t ~range = replace t ~range ~text:""

module Invalid_rewrites = struct
  type t =
    { path : Fpath.t
    ; rewrites_with_overlap : Rewrite.t list
    }

  let to_sexps { path; rewrites_with_overlap } =
    Sexplib0.Sexp.Atom (path |> Fpath.to_string)
    :: List.map Rewrite.sexp_of_t rewrites_with_overlap
  ;;

  let sexp_of_t t = Sexplib0.Sexp.List (to_sexps t)
end

exception Invalid_rewrites of Invalid_rewrites.t

let () =
  Sexplib0.Sexp_conv.Exn_converter.add [%extension_constructor Invalid_rewrites] (function
    | Invalid_rewrites t ->
      List (Atom "File_rewriter.Invalid_rewrites" :: Invalid_rewrites.to_sexps t)
    | _ -> assert false)
;;

let[@tail_mod_cons] rec rewrites_with_overlap current_offset = function
  | [] -> []
  | [ ({ Rewrite.start; stop = _; replace_by = _ } as rewrite) ] ->
    if current_offset > start then [ rewrite ] else []
  | a :: (b :: _ as tl) ->
    if current_offset > a.start || a.stop > b.start
    then a :: rewrites_with_overlap a.stop tl
    else rewrites_with_overlap a.stop tl
;;

let sorted_rewrites t =
  let rewrites = t.rewrites |> List.rev |> List.stable_sort Rewrite.compare in
  match rewrites_with_overlap 0 rewrites with
  | [] -> rewrites
  | _ :: _ as rewrites_with_overlap ->
    raise (Invalid_rewrites { path = t.path; rewrites_with_overlap })
;;

let contents t =
  let rewrites = sorted_rewrites t in
  let buffer = Buffer.create 13 in
  let length = String.length t.original_contents in
  let insert_original_contents ~from ~up_to =
    Buffer.add_substring buffer t.original_contents from (up_to - from)
  in
  let final_offset =
    List.fold_left
      (fun current_offset { Rewrite.start; stop; replace_by } ->
        if current_offset < start
        then insert_original_contents ~from:current_offset ~up_to:start;
        Buffer.add_string buffer replace_by;
        stop)
      0
      rewrites
  in
  (* Add the last portion of the original buffer. *)
  if final_offset < length then insert_original_contents ~from:final_offset ~up_to:length;
  Buffer.contents buffer
;;

let contents_result t =
  match contents t with
  | ok -> Ok ok
  | exception Invalid_rewrites error -> Error error
;;
