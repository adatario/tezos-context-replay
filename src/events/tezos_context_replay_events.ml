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

module Tracker = struct
  module Generic = struct
    type id = int64

    let encoding_id = Data_encoding.int64
  end

  module Index = Generic
  module Context = Generic
  module Tree = Generic
end

type ('input, 'output) fn = 'input * 'output

let fn input output = (input, output)

let encoding_fn input output =
  let open Data_encoding in
  tup2 input output

let encoding_key =
  let open Data_encoding in
  list string

module Sigs = Tezos_context_sigs.Context
module Context = Tezos_context_disk.Context

module Tree = struct
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

  let encoding_t =
    let open Data_encoding in
    let mem_tag = 0
    and mem_encoding =
      encoding_fn (tup2 Tracker.Tree.encoding_id encoding_key) bool
    in
    matching
      (function
        | Mem v -> matched mem_tag mem_encoding v
        | _ -> raise @@ Invalid_argument "blups")
      [
        case ~title:"mem" (Tag mem_tag) mem_encoding
          (function Mem v -> Some v | _ -> None)
          (fun v -> Mem v);
      ]
end

type init_args = {
  patch_context : bool; (* only record presence of patch_context *)
  readonly : bool;
  indexing_strategy : [ `Always | `Minimal ];
  index_log_size : int;
      (* don't record path of store *)
      (* path : string *)
}

let encoding_indexing_strategy =
  let open Data_encoding in
  let always_tag = 0 and always_encoding = constant "always" in
  let minimal_tag = 1 and minimal_encoding = constant "minimal" in
  matching
    (function
      | `Always -> matched always_tag always_encoding ()
      | `Minimal -> matched minimal_tag minimal_encoding ())
    [
      case ~title:"Always" (Tag always_tag) always_encoding
        (function `Always -> Some () | _ -> None)
        (fun () -> `Always);
      case ~title:"Minimal" (Tag minimal_tag) minimal_encoding
        (function `Minimal -> Some () | _ -> None)
        (fun () -> `Minimal);
    ]

let encoding_init_args =
  let open Data_encoding in
  conv
    (fun { patch_context; readonly; indexing_strategy; index_log_size } ->
      ( patch_context,
        readonly,
        indexing_strategy,
        Int32.of_int @@ index_log_size ))
    (fun (patch_context, readonly, indexing_strategy, index_log_size) ->
      {
        patch_context;
        readonly;
        indexing_strategy;
        index_log_size = Int32.to_int index_log_size;
      })
  @@ obj4 (req "patch_context" bool) (req "readonly" bool)
       (req "indexing_strategy" encoding_indexing_strategy)
       (req "indexing_log_size" int32)

type t =
  | Tree of Tree.t
  | Mem of (Tracker.Context.id * Context.key, bool) fn
  | Init of (init_args, Tracker.Index.id) fn

let encoding_t =
  let open Data_encoding in
  let tree_tag = 0 in
  let mem_tag = 1
  and mem_encoding =
    encoding_fn (tup2 Tracker.Context.encoding_id encoding_key) bool
  in
  let init_tag = 2
  and init_encoding =
    encoding_fn encoding_init_args Tracker.Index.encoding_id
  in
  matching
    (function
      | Tree tree -> matched tree_tag Tree.encoding_t tree
      | Mem v -> matched mem_tag mem_encoding v
      | Init v -> matched init_tag init_encoding v)
    [
      case ~title:"Tree" (Tag tree_tag) Tree.encoding_t
        (function Tree t -> Some t | _ -> None)
        (fun tree -> Tree tree);
      case ~title:"mem" (Tag mem_tag) mem_encoding
        (function Mem v -> Some v | _ -> None)
        (fun v -> Mem v);
      case ~title:"init" (Tag init_tag) init_encoding
        (function Init v -> Some v | _ -> None)
        (fun v -> Init v);
    ]

let pp ppf = function
  | Tree _tree -> Fmt.pf ppf "Tree"
  | Mem _ -> Fmt.pf ppf "mem"
  | Init _ -> Fmt.pf ppf "init"
