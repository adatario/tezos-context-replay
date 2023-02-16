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

open Lwt.Syntax

module Context = struct
  module Impl = Tezos_context_disk.Context
  module Events = Tezos_context_events

  module Recorder = struct
    include Internal_event.Simple
    include Events

    let section = [ "trace"; "node"; "context"; "disk" ]
    let level = Internal_event.Debug

    let call =
      declare_1 ~section ~level ~name:"tezos-context recorder"
        ~msg:"recording call to tezos-context" ~pp1:pp ("event", encoding_t)

    let record event = emit call event
  end

  type error += Cannot_create_file = Impl.Cannot_create_file
  type error += Cannot_open_file = Impl.Cannot_open_file
  type error += Cannot_find_protocol = Impl.Cannot_find_protocol
  type error += Suspicious_file = Impl.Suspicious_file
  type t = Events.Tracker.Context.t
  type context = t
  type tree = Events.Tracker.Tree.t
  type index = Events.Tracker.Index.t

  module Tree = struct
    type raw = Impl.Tree.raw
    type repo = Impl.Tree.repo

    (* [_o __ ___] - From nothing to tree *)

    let empty ctxt =
      let result = Impl.Tree.empty (Events.Tracker.Context.unwrap ctxt) in
      let wrapped_result = Events.Tracker.Tree.wrap result in
      let* () =
        Recorder.(
          record
          @@ Tree
               (Tree.Empty
                  (fn (Tracker.Context.id ctxt)
                     (Tracker.Tree.id wrapped_result))))
      in
      wrapped_result

    let of_raw x =
      let record_and_return_output =
        iter_recorders (fun (module R) -> R.Tree.of_raw x) (fun res -> !!res)
      in
      Impl.Tree.of_raw x |> Tree_traced.wrap |> record_and_return_output

    let of_value x y =
      let record_and_return_output =
        iter_recorders
          (fun (module R) -> R.Tree.of_value ~~x y)
          (fun res -> !!res)
      in
      Impl.Tree.of_value (Context_traced.unwrap x) y >|= fun res ->
      Tree_traced.wrap res |> record_and_return_output

    (* [i_ __ ___] - From tree to nothing *)

    let mem x y =
      let record_and_return_output =
        iter_recorders (fun (module R) -> R.Tree.mem !!x y) Fun.id
      in
      Impl.Tree.mem (Tree_traced.unwrap x) y >|= record_and_return_output

    let mem_tree x y =
      let record_and_return_output =
        iter_recorders (fun (module R) -> R.Tree.mem_tree !!x y) Fun.id
      in
      Impl.Tree.mem_tree (Tree_traced.unwrap x) y >|= record_and_return_output

    let find x y =
      let record_and_return_output =
        iter_recorders (fun (module R) -> R.Tree.find !!x y) Fun.id
      in
      Impl.Tree.find (Tree_traced.unwrap x) y >|= record_and_return_output

    let is_empty x =
      let record_and_return_output =
        iter_recorders (fun (module R) -> R.Tree.is_empty !!x) Fun.id
      in
      Impl.Tree.is_empty (Tree_traced.unwrap x) |> record_and_return_output

    let kind x =
      let record_and_return_output =
        iter_recorders (fun (module R) -> R.Tree.kind !!x) Fun.id
      in
      Impl.Tree.kind (Tree_traced.unwrap x) |> record_and_return_output

    let hash x =
      let record_and_return_output =
        iter_recorders (fun (module R) -> R.Tree.hash !!x) Fun.id
      in
      Impl.Tree.hash (Tree_traced.unwrap x) |> record_and_return_output

    let equal x y =
      let record_and_return_output =
        iter_recorders (fun (module R) -> R.Tree.equal !!x !!y) Fun.id
      in
      Impl.Tree.equal (Tree_traced.unwrap x) (Tree_traced.unwrap y)
      |> record_and_return_output

    let to_value x =
      let record_and_return_output =
        iter_recorders (fun (module R) -> R.Tree.to_value !!x) Fun.id
      in
      Impl.Tree.to_value (Tree_traced.unwrap x) >|= record_and_return_output

    let clear ?depth x =
      let record_and_return_output =
        iter_recorders (fun (module R) -> R.Tree.clear ~depth !!x) Fun.id
      in
      Impl.Tree.clear ?depth (Tree_traced.unwrap x) |> record_and_return_output

    (* [io __ ___] - From tree to tree *)

    let find_tree x y =
      let record_and_return_output =
        iter_recorders
          (fun (module R) -> R.Tree.find_tree !!x y)
          (Option.map ( !! ))
      in
      Impl.Tree.find_tree (Tree_traced.unwrap x) y >|= fun res ->
      Option.map Tree_traced.wrap res |> record_and_return_output

    let list x ?offset ?length y =
      let record_and_return_output =
        iter_recorders
          (fun (module R) -> R.Tree.list !!x ~offset ~length)
          (List.map (fun (step, tree) -> (step, !!tree)))
      in
      Impl.Tree.list (Tree_traced.unwrap x) ?offset ?length y >|= fun l ->
      List.map (fun (a, b) -> (a, Tree_traced.wrap b)) l
      |> record_and_return_output

    let add x y z =
      let record_and_return_output =
        iter_recorders (fun (module R) -> R.Tree.add !!x y z) ( !! )
      in
      Impl.Tree.add (Tree_traced.unwrap x) y z >|= fun res ->
      Tree_traced.wrap res |> record_and_return_output

    let add_tree x y z =
      let record_and_return_output =
        iter_recorders (fun (module R) -> R.Tree.add_tree !!x y !!z) ( !! )
      in
      Impl.Tree.add_tree (Tree_traced.unwrap x) y (Tree_traced.unwrap z)
      >|= fun res -> Tree_traced.wrap res |> record_and_return_output

    let remove x y =
      let record_and_return_output =
        iter_recorders (fun (module R) -> R.Tree.remove !!x y) ( !! )
      in
      Impl.Tree.remove (Tree_traced.unwrap x) y >|= fun res ->
      Tree_traced.wrap res |> record_and_return_output

    let fold ?depth x y ~order ~init ~f =
      let entry_count = ref 0 in
      let f a b c =
        let b = Tree_traced.wrap b in
        let record_and_return_output =
          iter_recorders
            (fun (module R) -> R.Tree.fold_step !entry_count !!b)
            Fun.id
        in
        f a b c >|= fun res ->
        record_and_return_output ();
        incr entry_count;
        res
      in
      let record_and_return_output =
        iter_recorders
          (fun (module R) -> R.Tree.fold ~depth ~order !!x y)
          Fun.id
      in
      Impl.Tree.fold ~order ?depth (Tree_traced.unwrap x) y ~init ~f
      >|= fun res ->
      let (_ : int) = record_and_return_output !entry_count in
      res

    (* Tracked with unhandled *)

    let shallow x y =
      record_unhandled_direct Recorder.Tree_shallow @@ fun () ->
      Impl.Tree.shallow x y |> Tree_traced.wrap

    let kinded_key x =
      record_unhandled_direct Recorder.Tree_kinded_key @@ fun () ->
      Impl.Tree.kinded_key (Tree_traced.unwrap x)

    let config x =
      record_unhandled_direct Recorder.Tree_config @@ fun () ->
      Impl.Tree.config (Tree_traced.unwrap x)

    let to_raw x =
      record_unhandled_lwt Recorder.Tree_to_raw
      @@ Impl.Tree.to_raw (Tree_traced.unwrap x)

    let pp x y =
      record_unhandled_direct Recorder.Tree_pp @@ fun () ->
      Impl.Tree.pp x (Tree_traced.unwrap y)

    let length x y =
      record_unhandled_lwt Recorder.Tree_length
      @@ Impl.Tree.length (Tree_traced.unwrap x) y

    (* Not tracked *)

    let is_shallow t = Impl.Tree.is_shallow (Tree_traced.unwrap t)
    let make_repo = Impl.Tree.make_repo
    let raw_encoding = Impl.Tree.raw_encoding

    let unshallow x =
      Impl.Tree.unshallow (Tree_traced.unwrap x) >|= Tree_traced.wrap
  end

  let mem ctxt key =
    let* result = Impl.mem ctxt key in
    let+ () = Recorder.(record @@ Mem (fn (0L, key) result)) in
    result

  (* Not recorded *)

  module Checks = Impl.Checks
  module Proof = Impl.Proof
end
