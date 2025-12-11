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

type t =
  { original_sexps : Sexp.t list
  ; positions : Parsexp.Positions.t
  ; file_rewriter : File_rewriter.t
  ; parser_result : Parsexp.Many_and_positions.parsed_value
  }

let reset { original_sexps = _; positions = _; file_rewriter; parser_result = _ } =
  File_rewriter.reset file_rewriter
;;

let path t = File_rewriter.path t.file_rewriter
let contents t = File_rewriter.contents t.file_rewriter
let contents_result t = File_rewriter.contents_result t.file_rewriter
let file_rewriter t = t.file_rewriter
let original_sexps t = t.original_sexps

module Position = struct
  let loc_of_parsexp_range ~path (range : Parsexp.Positions.range) =
    let source_code_position ({ line; col; offset } : Parsexp.Positions.pos) =
      { Lexing.pos_fname = path |> Fpath.to_string
      ; pos_lnum = line
      ; pos_cnum = offset
      ; pos_bol = offset - col
      }
    in
    Loc.create (source_code_position range.start_pos, source_code_position range.end_pos)
  ;;

  let loc t range = loc_of_parsexp_range ~path:(File_rewriter.path t.file_rewriter) range

  let range (range : Parsexp.Positions.range) =
    { Loc.Range.start = range.start_pos.offset; stop = range.end_pos.offset }
  ;;
end

exception
  Position_not_found of
    { positions : Parsexp.Positions.t
    ; sexp : Sexp.t
    }

let () =
  Sexplib0.Sexp_conv.Exn_converter.add
    [%extension_constructor Position_not_found]
    (function
    | Position_not_found { positions; sexp } ->
      List
        [ Atom "Sexps_rewriter.Position_not_found"
        ; positions |> Parsexp.Positions.sexp_of_t
        ; sexp
        ]
    | _ -> assert false)
;;

let position { positions; original_sexps; _ } sexp =
  match
    Parsexp.Positions.find_sub_sexp_in_list_phys positions original_sexps ~sub:sexp
  with
  | Some range -> range
  | None -> raise (Position_not_found { positions : Parsexp.Positions.t; sexp : Sexp.t })
;;

let loc t sexp = position t sexp |> Position.loc t
let range t sexp = position t sexp |> Position.range
let start_offset t sexp = (range t sexp).start
let stop_offset t sexp = (range t sexp).stop

module Visitor_decision = struct
  type t =
    | Break
    | Continue
    | Skip
end

let visit t ~f =
  let rec visit = function
    | [] -> ()
    | [] :: tl -> visit tl
    | (sub :: tl) :: rest ->
      (match
         (f sub ~range:(range t sub) ~file_rewriter:t.file_rewriter : Visitor_decision.t)
       with
       | Break -> ()
       | Skip -> visit (tl :: rest)
       | Continue ->
         (match sub with
          | Atom _ -> visit (tl :: rest)
          | List sexps -> visit (sexps :: tl :: rest)))
  in
  visit [ t.original_sexps ]
;;

module Parse_error = struct
  type t =
    { loc : Loc.t
    ; message : string
    }
end

let create ~path ~original_contents =
  match Parsexp.Many_and_positions.parse_string original_contents with
  | Ok ((original_sexps, positions) as parser_result) ->
    Ok
      { original_sexps
      ; positions
      ; file_rewriter = File_rewriter.create ~path ~original_contents
      ; parser_result
      }
  | Error parse_error ->
    let position = Parsexp.Parse_error.position parse_error in
    let message = Parsexp.Parse_error.message parse_error in
    let loc =
      Position.loc_of_parsexp_range ~path { start_pos = position; end_pos = position }
    in
    Error { Parse_error.loc; message }
;;

module Private = struct
  let parser_result t = t.parser_result
end
