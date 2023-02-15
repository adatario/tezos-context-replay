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

module type TRACKED = sig
  type t
  type unwrapped
  type id = int64

  val encoding_id : id Data_encoding.t
  val wrap : unwrapped -> t
  val unwrap : t -> unwrapped
  val id : t -> id
end

module Make (V : sig
  type t
end) =
struct
  type id = int64

  let encoding_id = Data_encoding.int64

  type unwrapped = V.t
  type t = { value : unwrapped; id : id }

  let counter = ref 0L

  let wrap value =
    let id = !counter in
    counter := Int64.succ !counter;
    { id; value }

  let unwrap { value; _ } = value
  let id { id; _ } = id
end

module Index = Make (struct
  type t = Tezos_context_disk.Context.index
end)

module Context = Make (struct
  type t = Tezos_context_disk.Context.context
end)

module Tree = Make (struct
  type t = Tezos_context_disk.Context.tree
end)
