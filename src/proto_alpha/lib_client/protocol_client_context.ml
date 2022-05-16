(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2018 Dynamic Ledger Solutions, Inc. <contact@tezos.com>     *)
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

module Lifted_protocol = struct
  include Protocol.Environment.Lift (Protocol)

  let hash = Protocol.hash
end

module Alpha_block_services =
  Block_services.Make (Lifted_protocol) (Lifted_protocol)

(** Client RPC context *)
class type rpc_context =
  object
    inherit RPC_context.generic

    inherit
      [Shell_services.chain * Shell_services.block] Protocol.Environment
                                                    .RPC_context
                                                    .simple
  end

(** The class [wrap_rpc_context] is a wrapper class used by the proxy
    mode clients. From a general-purpose RPC_context.generic [t], the
    class is augmented with shell services to provide RPC calls that
    are protocol-dependent. *)
class wrap_rpc_context (t : RPC_context.generic) : rpc_context =
  object
    method base : Uri.t = t#base

    method generic_media_type_call = t#generic_media_type_call

    method call_service
        : 'm 'p 'q 'i 'o.
          (([< Resto.meth] as 'm), unit, 'p, 'q, 'i, 'o) RPC_service.t ->
          'p ->
          'q ->
          'i ->
          'o tzresult Lwt.t =
      t#call_service

    method call_streamed_service
        : 'm 'p 'q 'i 'o.
          (([< Resto.meth] as 'm), unit, 'p, 'q, 'i, 'o) RPC_service.t ->
          on_chunk:('o -> unit) ->
          on_close:(unit -> unit) ->
          'p ->
          'q ->
          'i ->
          (unit -> unit) tzresult Lwt.t =
      t#call_streamed_service

    (** Abstracts variables <chain_id> and <block_id> in protocol RPCs
        prefixed by "/chains/<chain_id>/blocks/<block_id>/...". *)
    inherit
      [Shell_services.chain, Shell_services.block] Protocol.Environment
                                                   .proto_rpc_context
        (t :> RPC_context.t)
        Shell_services.Blocks.path
  end

(** The class type [full] allows to create contexts that are
    explicitly used by low-level shell functions, while containing
    various information (I/O services, RPCs...). Then, depending on the
    usage, the type may be coerced into one of its following ascendants
    to serve for explicit operations on blocks, chain or daemon for
    instance. *)
class type full =
  object
    (** The class Client_context.full provides I/O services for the
        client, the wallet, etc. *)
    inherit Client_context.full

    (** Base interface provided to call RPCs, i.e., communication
        with the node. A client context is defined by mapping all
        RPCs protocol-generic to a specific protocol. *)
    inherit
      [Shell_services.chain * Shell_services.block] Protocol.Environment
                                                    .RPC_context
                                                    .simple

    (** Protocol RPCs exposed through the environment (using
        an additional chainpath). *)
    inherit
      [Shell_services.chain, Shell_services.block] Protocol.Environment
                                                   .proto_rpc_context
  end

(** From a [Client_context.full], the class allows to call RPCs from
    the node and those defined by the protocol. *)
class wrap_full (t : Client_context.full) : full =
  object
    inherit Client_context.proxy_context t

    inherit
      [Shell_services.chain, Shell_services.block] Protocol.Environment
                                                   .proto_rpc_context
        (t :> RPC_context.t)
        Shell_services.Blocks.path
  end

let register_error_kind category ~id ~title ~description ?pp encoding from_error
    to_error =
  let id = "client." ^ Protocol.name ^ "." ^ id in
  register_error_kind
    category
    ~id
    ~title
    ~description
    ?pp
    encoding
    from_error
    to_error

(** Initialization calls that run on start-up. Register the various
    protocol encodings. *)
let () =
  let open Data_encoding.Registration in
  register Protocol.Alpha_context.Lazy_storage.encoding ;
  register ~pp:Protocol.Alpha_context.Fitness.pp
  @@ Protocol.Alpha_context.Fitness.encoding ;
  (* These encodings are missing a def field which we add before registering them.
     These defs should be moved inside their encodings in the protocol code. *)
  let def id ids ?title ?description encoding =
    Data_encoding.def
      (String.concat "." (Protocol.name :: id :: ids))
      ?title
      ?description
      encoding
  in
  register @@ def "parameters" [] Protocol.Parameters_repr.encoding ;
  register ~pp:Protocol.Alpha_context.Tez.pp
  @@ def "tez" [] Protocol.Alpha_context.Tez.encoding ;
  register ~pp:Protocol.Alpha_context.Timestamp.pp
  @@ def "timestamp" [] Protocol.Alpha_context.Timestamp.encoding ;
  register ~pp:Protocol.Alpha_context.Raw_level.pp
  @@ def "raw_level" [] Protocol.Alpha_context.Raw_level.encoding ;
  register @@ def "vote" ["ballot"] Protocol.Alpha_context.Vote.ballot_encoding ;
  register
  @@ def "vote" ["ballots"] Protocol.Alpha_context.Vote.ballots_encoding ;
  register
  @@ def "vote" ["listings"] Protocol.Alpha_context.Vote.listings_encoding ;
  register @@ def "seed" [] Protocol.Alpha_context.Seed.seed_encoding ;
  register ~pp:Protocol.Alpha_context.Gas.pp
  @@ def "gas" [] Protocol.Alpha_context.Gas.encoding ;
  register ~pp:Protocol.Alpha_context.Gas.pp_cost
  @@ def "gas" ["cost"] Protocol.Alpha_context.Gas.cost_encoding ;
  register @@ def "script" [] Protocol.Alpha_context.Script.encoding ;
  register @@ def "script" ["expr"] Protocol.Alpha_context.Script.expr_encoding ;
  register @@ def "script" ["prim"] Protocol.Alpha_context.Script.prim_encoding ;
  register
  @@ def "script" ["lazy_expr"] Protocol.Alpha_context.Script.lazy_expr_encoding ;
  register
  @@ def "script" ["loc"] Protocol.Alpha_context.Script.location_encoding ;
  register ~pp:Protocol.Alpha_context.Contract.pp
  @@ def "contract" [] Protocol.Alpha_context.Contract.encoding ;
  register
  @@ def
       "receipt"
       ["balance_updates"]
       Protocol.Alpha_context.Receipt.balance_updates_encoding ;
  register ~pp:Protocol.Alpha_context.Level.pp_full
  @@ def "level" [] Protocol.Alpha_context.Level.encoding ;
  register @@ def "operation" [] Protocol.Alpha_context.Operation.encoding ;
  register
  @@ def
       "operation"
       ["contents"]
       Protocol.Alpha_context.Operation.contents_encoding ;
  register
  @@ def
       "operation"
       ["contents_list"]
       Protocol.Alpha_context.Operation.contents_list_encoding ;
  register
  @@ def
       "operation"
       ["protocol_data"]
       Protocol.Alpha_context.Operation.protocol_data_encoding ;
  register
  @@ def "operation" ["raw"] Protocol.Alpha_context.Operation.raw_encoding ;
  register
  @@ def
       "operation"
       ["internal"]
       Protocol.Apply_results.internal_contents_encoding ;
  register
  @@ def
       "operation"
       ["unsigned"]
       Protocol.Alpha_context.Operation.unsigned_encoding ;
  register ~pp:Protocol.Alpha_context.Period.pp
  @@ def "period" [] Protocol.Alpha_context.Period.encoding ;
  register ~pp:Protocol.Alpha_context.Cycle.pp
  @@ def "cycle" [] Protocol.Alpha_context.Cycle.encoding ;
  register @@ def "constants" [] Protocol.Alpha_context.Constants.encoding ;
  register
  @@ def "constants" ["fixed"] Protocol.Alpha_context.Constants.fixed_encoding ;
  register
  @@ def
       "constants"
       ["parametric"]
       Protocol.Alpha_context.Constants.Parametric.encoding ;
  register @@ def "nonce" [] Protocol.Alpha_context.Nonce.encoding ;
  register @@ def "block_header" [] Protocol.Alpha_context.Block_header.encoding ;
  register
  @@ def
       "block_header"
       ["unsigned"]
       Protocol.Alpha_context.Block_header.unsigned_encoding ;
  register
  @@ def "block_header" ["raw"] Protocol.Alpha_context.Block_header.raw_encoding ;
  register
  @@ def
       "block_header"
       ["contents"]
       Protocol.Alpha_context.Block_header.contents_encoding ;
  register
  @@ def
       "block_header"
       ["shell_header"]
       Protocol.Alpha_context.Block_header.shell_header_encoding ;
  register
  @@ def
       "block_header"
       ["protocol_data"]
       Protocol.Alpha_context.Block_header.protocol_data_encoding ;
  register ~pp:Protocol.Alpha_context.Voting_period.pp
  @@ def "voting_period" [] Protocol.Alpha_context.Voting_period.encoding ;
  register
  @@ def
       "voting_period"
       ["kind"]
       Protocol.Alpha_context.Voting_period.kind_encoding ;
  register
  @@ def
       "errors"
       []
       ~description:
         "The full list of RPC errors would be too long to include.It is\n\
          available through the RPC `/errors` (GET)."
       error_encoding
