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

(** A file rewriter is an abstraction used by programs that wants to perform
    some tweaks to an existing file, such as small rewrites or refactoring.

    The way they express their changes is by adding substitution to an existing
    file contents. Add the end, all substitutions are applied and a new contents
    can be produced. *)

(** Number of bytes from the beginning of the input. The first byte has offset
    [0]. *)
type offset := Loc.Offset.t

(** A range refers to a chunk of the file, from start (included) to stop
    (excluded). Ranges may be used to remove chunks of the file, or replace
    them with some alternative text. *)
type range := Loc.Range.t

(** [t] is a mutable type that holds the original contents of the file being
    rewritten, as well as all the edits that are enqueued to it over time
    (inserts, replaces and removals). *)
type t

(** Initialize a new [t] from an original contents, initially with no edits. The
    [path] supplied is only used for context awareness and error messages, but
    no I/O occurs on disk. *)
val create : path:Fpath.t -> original_contents:string -> t

(** {1 Rewrites} *)

(** This may come as a surprise, but offsets refer to the offsets in the
    original contents, not the ongoing rewritten buffer. Thus, it is OK to
    insert several texts at the same offset. The order of insertion decides
    which inserts are applied first. Raises [Invalid_argument] if [offset] is
    not a valid offset in t's [original_contents], indicating a programming
    error that needs to be fixed. *)
val insert : t -> offset:offset -> text:string -> unit

(** Replace an entire section denoted by the given range by an alternative text.
    The range refers to the range in the original contents, not the ongoing
    rewritten buffer. Through the use of this module it is invalid to request
    [replaces] with overlapping ranges, and doing so will cause exception
    {!exception:Invalid_rewrites} to be raised during [contents]. Raises
    [Invalid_argument] if [range] is not a valid range in t's
    [original_contents], indicating a programming error that needs to be
    fixed. *)
val replace : t -> range:range -> text:string -> unit

(** This is a small convenient wrapper that is equivalent to replacing a range
    by the empty string. Raises [Invalid_argument] if [range] is not a valid
    range in t's [original_contents], indicating a programming error that
    needs to be fixed. *)
val remove : t -> range:range -> unit

(** You may decide at a given moment that you want to discard all the edits
    inserted so far, and just start over. This has the same effect than
    starting fresh with a newly created [t]. *)
val reset : t -> unit

(** {1 Output} *)

module Invalid_rewrites : sig
  type t

  val sexp_of_t : t -> Sexplib0.Sexp.t
end

exception Invalid_rewrites of Invalid_rewrites.t

(** Build the final result, with all the substitutions applied. If the
    substitutions requested causes conflicts, this function will raise
    {!exception:Invalid_rewrites}. The exception is not really meant to be
    caught in a production path, because it would usually be indicative of a
    programming error. If you prefer a result type, see
    {!val:contents_result}. *)
val contents : t -> string

(** Same as {!val:contents} but wraps the output into a result type rather than
    raising an exception. *)
val contents_result : t -> (string, Invalid_rewrites.t) Result.t

(** {1 Getters} *)

(** Returns the path that was supplied to [create]. *)
val path : t -> Fpath.t

(** Returns the original contents that was supplied to [create]. *)
val original_contents : t -> string
