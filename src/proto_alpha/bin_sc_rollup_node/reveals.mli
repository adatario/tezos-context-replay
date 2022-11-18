(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2022 Nomadic Labs, <contact@nomadic-labs.com>               *)
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

(** This module provides basic support for reveals.

    The rollup can ask for data being the reveal of some hash. This
    allows transferring data directly to the rollup without going
    through the L1 inbox.

    Data length must be under 4KB to be refutable in a single L1
    operation.

    Data must be made available by off-chain mechanisms: it is the
    responsibility of the rollup kernel to make sure that the reveal
    data is available: otherwise, there is a potential safety issue.

    For the moment, the support is basic and mostly manual as the operator
    needs to explicitly import a file in the rollup node data directoy to
    enable the rollup node to answer reveal requests.

*)

(* FIXME:https://gitlab.com/tezos/tezos/-/issues/3854

   We should probably have a mechanism to let the kernel declare
   sources of reveal data so that the rollup node can automatically
   download data in advance. *)

open Protocol.Alpha_context

(** Source of data  *)
type source =
  | String of string  (** A string containing the whole data *)
  | File of string
      (** A file name whose associated file contains the whole data *)

(** [get ~data_dir ~pvm_name ~hash] retrieves the data associated with
    the reveal hash [hash] from disk. May fail with:
    {ul
      {li [Wrong_hash {found; expected}] where [expected = hash], and
        [found <> hash], if the data is retrieved and hashes to the wrong
        hash [found],}
     {li [Could_not_open_preimage_file filename] if the function tries to
        retrieve the data from [filename], but it cannot read the contents
        of the file.}
   } *)
val get :
  data_dir:string ->
  pvm_name:string ->
  hash:Sc_rollup.Reveal_hash.t ->
  string tzresult Lwt.t

(** [import ~data_dir pvm_kind ~filename] turns the content of ~filename into a
    chunk of pages of (at most) 4KB and stores them on disk in [data_dir]. It
    returns the hash of the first chunk. *)
val import :
  data_dir:string ->
  Sc_rollup.Kind.t ->
  filename:string ->
  Sc_rollup.Reveal_hash.t tzresult

(** [chunkify pvm_kind source] turns the content of ~filename into a chunk of
    pages of (at most) 4KB. It returns the map of chunks and the hash of the
    first chunk. *)
val chunkify :
  Sc_rollup.Kind.t ->
  source ->
  (string Sc_rollup.Reveal_hash.Map.t * Sc_rollup.Reveal_hash.t) tzresult
