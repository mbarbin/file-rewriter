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

(** This is thin wrapper on top of {!File_rewriter} that specializes in
    applying rewrites to sexp files.

    The primary motivation for which this library is created is to automatically
    apply some linting rules to the lots of dune files present in a large
    monorepo.

    A note on editing files with a single sexp (rather than a list of them): it
    should just work, since one sexp is just a sub case of many sexps. *)

type t

module Parse_error : sig
  (** When creating a rewriter, we first parse sexps from the original contents
      provided. This may result in parsing errors.

      The type is intended to make it easy to connect to code using [Err], such
      as shown below:

      {[
        match Sexps_rewriter.create ~path ~original_contents with
        | Ok r -> r
        | Error { loc; message } -> Err.raise ~loc [ Pp.text message ]
      ]} *)
  type t =
    { loc : Loc.t
    ; message : string
    }
end

(** A [t] shall be created from a string that would yield a successful parsing
    of many sexps and positions from a file, using the [Parsexp] library. The
    [path] is provided to create a [Loc.t] for error messages, but no I/O is
    actually performed on disk - the sexps are parsed from the string provided
    by the parameter [original_contents]. *)
val create : path:Fpath.t -> original_contents:string -> (t, Parse_error.t) Result.t

(** {1 Rewrites} *)

module Visitor_decision : sig
  (** While we visit the sexps from the original file, we may decide what to do
      at each step of the iteration:

      - Do we want to stop the execution there?
      - Or do we want to recurse with the children of the current sexp
      - Or simply go to the next one sexp in the queue? *)

  type t =
    | Break (** Stops the current execution of [visit]. *)
    | Continue
    (** Recurse and visit the children of the current sexp if any, or
        continue with the next sexp in the queue. *)
    | Skip
    (** Do not drill in, skip the current sexp and continue with the next
        sexp in the queue. If the current sexp is an atom, this is
        equivalent to [Continue], which should be preferred by default. *)
end

(** Visiting the input, perhaps recursively. Unless you are breaking the
    execution with [Break] or [Skip], the function [f] provided will be called
    on all the sexps (including recursively on all internal ones) of the
    input.

    The expected way to use this function is to do some pattern matching on the
    [Sexp.t] currently at hand, and use the [File_rewriter] api if some rewrites
    are required. If the current sexp is not one you are targeting you may
    simply ignore it and return [Continue], to be called again on other parts of
    the input.

    Note that you may visit the same input multiple times (that is, calling
    [visit] multiple times, with different invocations of [f]), however, be
    mindful that the [file_rewriter] that you manipulate is the same each
    time, thus the final computation of the output will fail if you enqueue
    incompatible rewrites in it. *)
val visit
  :  t
  -> f:
       (Sexp.t
        -> range:Loc.Range.t
        -> file_rewriter:File_rewriter.t
        -> Visitor_decision.t)
  -> unit

(** You may decide at a given moment that you want to discard all the rewrites
    done so far, and just start over. This has the same effect than
    starting fresh with a newly created [t]. *)
val reset : t -> unit

(** {1 Output} *)

(** Produce the resulting buffer, with all the rewrites applied. Note that [t]
    may continue to be used further from here, and you can call [contents]
    again later. This raises {!exception:File_rewriter.Invalid_rewrites} if
    inconsistent rewrites have been submitted to t's [file_rewriter]. *)
val contents : t -> string

(** Same as {!val:contents} but wraps the output into a result type rather than
    raising an exception. *)
val contents_result : t -> (string, File_rewriter.Invalid_rewrites.t) Result.t

(** {1 Getters} *)

(** If you need access to the internal file_rewriter, this accessor is
    exposed. *)
val file_rewriter : t -> File_rewriter.t

(** Access the raw sexps that were parsed from the original contents. *)
val original_sexps : t -> Sexp.t list

(** Retrieve the path provided when [t] was created. *)
val path : t -> Fpath.t

(** {1 Utils on positions and ranges} *)

module Position : sig
  (** Transform a range into a loc, for example to report an error with
      [Error_log]. *)
  val loc : t -> Parsexp.Positions.range -> Loc.t

  (** To be used in conjunctions with [File_rewriter]. *)
  val range : Parsexp.Positions.range -> Loc.Range.t
end

(** An exception raised when trying to locate the position of an unknown [sexp]
    when traversing the input. We don't expose functions returning results
    rather than raising this exception in this interface because we can't
    think of an actual use case for the erroring case. If you are trying to
    locate positions from an invalid sexp, we envision that this is the result
    of a programming error which should be fixed. *)
exception
  Position_not_found of
    { positions : Parsexp.Positions.t
    ; sexp : Sexp.t
    }

(** Assuming the supplied [Sexp.t] is phys_equal to a [Sexp.t] found during a
    call to [visit], the function [position] will be able to return its
    position. This is using [Parsexp.Positions.t] under the hood, which keeps
    track of the positions of all the sexps that have been parsed. If the
    [Sexp.t] cannot be found as phys_equal to one of the parsed ones, this
    function will raise {!exception:Position_not_found}. *)
val position : t -> Sexp.t -> Parsexp.Positions.range

(** This is just a shortcut to calling [position] and converting its result with
    [Position.loc]. It raises if [position] raises. *)
val loc : t -> Sexp.t -> Loc.t

(** This is just a shortcut to calling [position] and converting its result with
    [Position.range]. It raises if [position] raises. *)
val range : t -> Sexp.t -> Loc.Range.t

val start_offset : t -> Sexp.t -> Loc.Offset.t
val stop_offset : t -> Sexp.t -> Loc.Offset.t

module Private : sig
  (** If the current API does not suffice to cover your use case, you may access
      the internal sexps and positions computed from the parsing of the original
      content. We offer less stability on this part of the API, just in case we
      decide the change the internal parsing engine used by this module. *)
  val parser_result : t -> Parsexp.Many_and_positions.parsed_value
end
