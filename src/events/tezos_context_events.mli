(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2021-2023 Tarides <contact@tarides.com>                     *)
(*                                                                           *)
(* Permission is hereby granted, free of charge, to any person obtaining a   *)
(* copy of this software and associated documentation files (the "Software"),*)
(* to deal in the Software without restriction, including without limitation *)
(* the rights to use, copy, modify, merge, publish, distribute, sublicense,  *)
(* and/or sell copies of the Software, and to permit persons to whom the     *)
(* Software is furnished to do so, subject to the following conditions:      *)
(*                                                                           *)
(* The above copyright notice and this permission notice shall be included   *)
(* in all copies or substantial portions of the Software.                    *)
(*                                                                           *)
(* THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR*)
(* IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,  *)
(* FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL   *)
(* THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER*)
(* LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING   *)
(* FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER       *)
(* DEALINGS IN THE SOFTWARE.                                                 *)
(*                                                                           *)
(*****************************************************************************)

(** {1 Tezos Context Events}

    This library defines events that log calls to the tezos-context API.

    A serialization of the events is provided using {!Data_encoding}.
 *)

module Sigs = Tezos_context_sigs.Context
module Context = Tezos_context_disk.Context

(** {2 Tracker}

    Certain arguments or return values to the Tezos Context API are
    too big to efficiently log ([Context.t], [Context.index] and
    [Context.tree]). Instead of logging an exact representation of
    such values, we assign every instance of such a type an identifier
    that is logged. As creation and mutation of such values goes
    exclusively trough the [tezos-context] API, we can track all
    instances of objects and can re-use the identiiers when replaying
    a trace of events.

 *)

module Tracker : sig
  module type TRACKED = sig
    type t
    type unwrapped

    (* TODO: make the id type opaque *)
    type id = int64

    val encoding_id : id Data_encoding.t
    val wrap : unwrapped -> t
    val unwrap : t -> unwrapped
    val id : t -> id
  end

  module Index : TRACKED with type unwrapped = Context.index
  module Tree : TRACKED with type unwrapped = Context.tree
  module Context : TRACKED with type unwrapped = Context.context
end

(** {2 Events} *)

type ('input, 'output) fn

val fn : 'input -> 'output -> ('input, 'output) fn

val encoding_fn :
  'input Data_encoding.t ->
  'output Data_encoding.t ->
  ('input, 'output) fn Data_encoding.t

module Tree : sig
  type list_args = {
    tree : Tracker.Tree.id;
    offset : int option;
    length : int option;
    key : Context.key;
  }

  type t =
    (* Getters - defined in VIEW *)
    | Mem of (Tracker.Tree.id * Context.key, bool) fn
    | Mem_tree of (Tracker.Tree.id * Context.key, bool) fn
    | Find of (Tracker.Tree.id * Context.key, Context.value option) fn
    | Find_tree of (Tracker.Tree.id * Context.key, Tracker.Tree.id option) fn
    | List of (list_args, (string * Tracker.Tree.id) list) fn
    | Length of (Tracker.Tree.id * Context.key, int) fn
    (* Setters - defined in VIEW *)
    | Add of (Tracker.Tree.id * Context.key * Context.value, Tracker.Tree.id) fn
    | Add_tree of
        (Tracker.Tree.id * Context.key * Tracker.Tree.id, Tracker.Tree.id) fn
    | Remove of (Tracker.Tree.id * Context.key, Tracker.Tree.id) fn
    (* Folding - defined in VIEW *)
    | Fold (* not recorded *)
    (* Functions - defined in TREE *)
    | Empty of (Tracker.Context.id, Tracker.Tree.id) fn
    | Is_empty of (Tracker.Tree.id, bool) fn
    | Kind of (Tracker.Tree.id, Sigs.Kind.t) fn
    | To_value of (Tracker.Tree.id, Context.value) fn
    | Of_value of (Context.value, Tracker.Tree.id) fn
    | Hash of (Tracker.Tree.id, Context_hash.t) fn
    | Equal of (Tracker.Tree.id * Tracker.Tree.id, bool) fn
    | Clear of (int option * Tracker.Tree.id, unit) fn

  val encoding_t : t Data_encoding.t
end

type init_args = {
  patch_context : bool; (* only record presence of patch_context *)
  readonly : bool;
  indexing_strategy : [ `Always | `Minimal ];
  index_log_size : int;
      (* don't record path of store *)
      (* path : string *)
}

type t =
  | Tree of Tree.t
  | Mem of (Tracker.Context.id * Context.key, bool) fn
  | Init of (init_args, Tracker.Index.id) fn

val encoding_t : t Data_encoding.t
val pp : t Fmt.t
