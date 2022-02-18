(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2018 Dynamic Ledger Solutions, Inc. <contact@tezos.com>     *)
(* Copyright (c) 2018-2021 Nomadic Labs, <contact@nomadic-labs.com>          *)
(* Copyright (c) 2020 Metastate AG <hello@metastate.dev>                     *)
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

open Block_validator_worker_state
open Block_validator_errors

type limits = {protocol_timeout : Time.System.Span.t}

type result =
  | Already_commited
  | Outdated_block
  | Validated
  | Validation_error of error trace
  | Preapplied of (Block_header.shell_header * error Preapply_result.t list)
  | Preapplication_error of error trace
  | Validation_error_after_precheck of error trace
  | Precheck_failed of error trace

module Block_hash_ring =
  (val Ringo.(map_maker ~replacement:FIFO ~overflow:Strong ~accounting:Precise))
    (Block_hash)

module Name = struct
  type t = unit

  let encoding = Data_encoding.empty

  let base = ["validator"; "block"]

  let pp _ () = ()

  let equal () () = true
end

module Types = struct
  type metrics = {
    already_commited_blocks_count : Prometheus.Counter.t;
    outdated_blocks_count : Prometheus.Counter.t;
    validated_blocks_count : Prometheus.Counter.t;
    validation_errors_count : Prometheus.Counter.t;
    preapplied_blocks_count : Prometheus.Counter.t;
    preapplication_errors_count : Prometheus.Counter.t;
    validation_errors_after_precheck_count : Prometheus.Counter.t;
    precheck_failed_count : Prometheus.Counter.t;
    validation_worker_metrics : Worker_metrics.t;
  }

  type state = {
    protocol_validator : Protocol_validator.t;
    validation_process : Block_validator_process.t;
    limits : limits;
    start_testchain : bool;
    invalid_blocks_after_precheck : error trace Block_hash_ring.t;
  }

  type parameters = limits * bool * Distributed_db.t * Block_validator_process.t
end

module Request = struct
  include Request

  type validation_request = {
    chain_db : Distributed_db.chain_db;
    notify_new_block : Store.Block.t -> unit;
    canceler : Lwt_canceler.t option;
    peer : P2p_peer.Id.t option;
    hash : Block_hash.t;
    header : Block_header.t;
    operations : Operation.t list list;
    precheck_and_notify : bool;
  }

  type preapplication_request = {
    chain_store : Store.chain_store;
    canceler : Lwt_canceler.t option;
    predecessor : Store.Block.t;
    timestamp : Time.Protocol.t;
    protocol_data : bytes;
    operations : Operation.t list list;
  }

  type 'a t =
    | Request_validation : validation_request -> result t
    | Request_preapplication : preapplication_request -> result t

  let view : type a. a t -> view = function
    | Request_validation {chain_db; peer; hash; _} ->
        let chain_store = Distributed_db.chain_store chain_db in
        let chain_id = Store.Chain.chain_id chain_store in
        Validation {chain_id; block = hash; peer}
    | Request_preapplication {chain_store; predecessor; _} ->
        let chain_id = Store.Chain.chain_id chain_store in
        let level = Int32.succ (Store.Block.level predecessor) in
        Preapplication {chain_id; level}
end

module Logger =
  Worker_logger.Make (Event) (Request)
    (struct
      let worker_name = "node_block_validator"
    end)

module Worker = Worker.Make (Name) (Event) (Request) (Types) (Logger)

type t = Worker.infinite Worker.queue Worker.t

let check_chain_liveness chain_db hash (header : Block_header.t) =
  let chain_store = Distributed_db.chain_store chain_db in
  match Store.Chain.expiration chain_store with
  | Some eol when Time.Protocol.(eol <= header.shell.timestamp) ->
      fail @@ invalid_block hash
      @@ Expired_chain
           {
             chain_id = Store.Chain.chain_id chain_store;
             expiration = eol;
             timestamp = header.shell.timestamp;
           }
  | None | Some _ -> return_unit

let is_already_validated chain_store hash =
  Store.Block.is_known chain_store hash

let precheck_block bvp chain_store ~predecessor block_header block_hash
    operations =
  Block_validator_process.precheck_block
    bvp
    chain_store
    ~predecessor
    block_header
    block_hash
    operations

let precheck_block_and_advertise bvp chain_db chain_store ~predecessor hash
    header operations =
  precheck_block bvp chain_store ~predecessor header hash operations
  >>=? fun () ->
  (* Add the block and operations to the cache of the ddb to make them
     available to our peers *)
  Distributed_db.inject_prechecked_block chain_db hash header operations
  >>=? fun () ->
  (* Headers which have been preapplied can be advertised before being fully applied. *)
  Distributed_db.Advertise.prechecked_head chain_db header ;
  return_unit

let on_validation_request w
    {
      Request.chain_db;
      notify_new_block;
      canceler;
      peer;
      hash;
      header;
      operations;
      precheck_and_notify;
    } =
  let bv = Worker.state w in
  let chain_store = Distributed_db.chain_store chain_db in
  is_already_validated chain_store hash >>= function
  | true -> return Already_commited
  | false -> (
      (match Block_hash_ring.find_opt bv.invalid_blocks_after_precheck hash with
      | Some errs ->
          (* If the block is invalid but has been previously
             successfuly prechecked, we directly return with the cached
             errors. This way, multiple propagation won't happen. *)
          return (Validation_error_after_precheck errs)
      | None -> (
          Store.Block.read_invalid_block_opt chain_store hash >>= function
          | Some {errors; _} -> return (Validation_error errors)
          | None -> (
              Store.Chain.checkpoint chain_store >>= fun checkpoint ->
              (* Safety and late workers in partial mode. *)
              if Compare.Int32.(header.shell.level < snd checkpoint) then
                return Outdated_block
              else
                Store.Block.read_block chain_store header.shell.predecessor
                >>=? fun pred ->
                (if precheck_and_notify then
                 Worker.log_event w (Prechecking_block hash) >>= fun () ->
                 precheck_block_and_advertise
                   bv.validation_process
                   chain_db
                   chain_store
                   ~predecessor:pred
                   hash
                   header
                   operations
                 >>=? fun () ->
                 Worker.log_event w (Prechecked_block hash) >|= ok
                else return_unit)
                >>= function
                | Error errs ->
                    assert precheck_and_notify ;
                    return (Precheck_failed errs)
                | Ok () -> (
                    Worker.protect w (fun () ->
                        protect ?canceler (fun () ->
                            Worker.log_event w (Validating_block hash)
                            >>= fun () ->
                            Block_validator_process.apply_block
                              bv.validation_process
                              chain_store
                              ~predecessor:pred
                              header
                              operations
                            >>= function
                            | Ok x -> return x
                            (* [Unavailable_protocol] is expected to be the
                               first error in the trace *)
                            | Error (Unavailable_protocol {protocol; _} :: _) ->
                                Protocol_validator.fetch_and_compile_protocol
                                  bv.protocol_validator
                                  ?peer
                                  ~timeout:bv.limits.protocol_timeout
                                  protocol
                                >>=? fun _ ->
                                (* Retry validating after fetching the protocol *)
                                Block_validator_process.apply_block
                                  bv.validation_process
                                  chain_store
                                  ~predecessor:pred
                                  header
                                  operations
                            | Error _ as x -> Lwt.return x))
                    >>=? fun result ->
                    Distributed_db.commit_block
                      chain_db
                      hash
                      header
                      operations
                      result
                    >>=? function
                    | Some block ->
                        notify_new_block block ;
                        return Validated
                    | None -> return Already_commited))))
      >>= function
      | Ok r -> return r
      | Error errs ->
          (if List.exists (function Invalid_block _ -> true | _ -> false) errs
          then
           Worker.protect w (fun () ->
               Distributed_db.commit_invalid_block chain_db hash header errs)
          else return_unit)
          >>=? fun () ->
          if precheck_and_notify then (
            Block_hash_ring.replace bv.invalid_blocks_after_precheck hash errs ;
            return (Validation_error_after_precheck errs))
          else return (Validation_error errs))

let on_preapplication_request w
    {
      Request.chain_store;
      canceler;
      predecessor;
      timestamp;
      protocol_data;
      operations;
    } =
  let bv = Worker.state w in
  Worker.protect w (fun () ->
      protect ?canceler (fun () ->
          Block_validator_process.preapply_block
            bv.validation_process
            chain_store
            ~predecessor
            ~protocol_data
            ~timestamp
            operations))
  >>= function
  | Ok res -> return (Preapplied res)
  | Error errs -> return (Preapplication_error errs)

let on_request : type r. t -> r Request.t -> r tzresult Lwt.t =
 fun w -> function
  | Request.Request_validation r -> on_validation_request w r
  | Request.Request_preapplication r -> on_preapplication_request w r

let metrics =
  let namespace = String.concat "_" Name.base in
  let subsystem = None in
  let already_commited_blocks_count =
    let help = "Number of requests to validate a block already handled" in
    Prometheus.Counter.v
      ~help
      ~namespace
      ?subsystem
      "already_commited_blocks_count"
  in
  let outdated_blocks_count =
    let help =
      "Number of requests to validate a block older than the node's checkpoint"
    in
    Prometheus.Counter.v ~help ~namespace ?subsystem "outdated_blocks_count"
  in
  let validated_blocks_count =
    let help = "Number of requests to validate a valid block" in
    Prometheus.Counter.v ~help ~namespace ?subsystem "validated_blocks_count"
  in
  let validation_errors_count =
    let help = "Number of requests to validate an invalid block" in
    Prometheus.Counter.v ~help ~namespace ?subsystem "validation_errors_count"
  in
  let preapplied_blocks_count =
    let help = "Number of successful application simulations of blocks" in
    Prometheus.Counter.v ~help ~namespace ?subsystem "preapplied_blocks_count"
  in
  let preapplication_errors_count =
    let help = "Number of refused application simulations of blocks" in
    Prometheus.Counter.v
      ~help
      ~namespace
      ?subsystem
      "preapplication_errors_count"
  in
  let validation_errors_after_precheck_count =
    let help =
      "Number of requests to validate an invalid but precheckable block"
    in
    Prometheus.Counter.v
      ~help
      ~namespace
      ?subsystem
      "validation_errors_after_precheck_count"
  in
  let precheck_failed_count =
    let help =
      "Number of block validation requests where the prechecking of a block \
       failed"
    in
    Prometheus.Counter.v ~help ~namespace ?subsystem "precheck_failed_count"
  in
  let validation_worker_metrics =
    Worker_metrics.declare ~label_names:[] ~namespace ?subsystem () []
  in
  Types.
    {
      already_commited_blocks_count;
      outdated_blocks_count;
      validated_blocks_count;
      validation_errors_count;
      preapplied_blocks_count;
      preapplication_errors_count;
      validation_errors_after_precheck_count;
      precheck_failed_count;
      validation_worker_metrics;
    }

let on_launch _ _ (limits, start_testchain, db, validation_process) =
  let protocol_validator = Protocol_validator.create db in
  let invalid_blocks_after_precheck = Block_hash_ring.create 50 in
  return
    {
      Types.protocol_validator;
      validation_process;
      limits;
      start_testchain;
      invalid_blocks_after_precheck;
    }

let on_error w r st errs =
  match r with
  | Request.Validation v ->
      Worker.log_event w (Validation_failure (v, st, errs)) >>= fun () ->
      (* Keep the worker alive. *)
      return_unit
  | Preapplication v ->
      Worker.log_event w (Preapplication_failure (v, st, errs)) >>= fun () ->
      (* Keep the worker alive. *)
      return_unit

let on_completion :
    type a. t -> a Request.t -> a -> Worker_types.request_status -> unit Lwt.t =
 fun w request v st ->
  match (request, v) with
  | (Request.Request_validation {hash; _}, Already_commited) ->
      Prometheus.Counter.inc_one metrics.already_commited_blocks_count ;
      Worker.log_event w (Previously_validated hash) >>= fun () ->
      Lwt.return_unit
  | (Request.Request_validation {hash; _}, Outdated_block) ->
      Prometheus.Counter.inc_one metrics.outdated_blocks_count ;
      Worker.log_event w (Previously_validated hash) >>= fun () ->
      Lwt.return_unit
  | (Request.Request_validation _, Validated) -> (
      let () = Worker_metrics.update metrics.validation_worker_metrics st in
      Prometheus.Counter.inc_one metrics.validated_blocks_count ;
      match Request.view request with
      | Validation v -> Worker.log_event w (Validation_success (v, st))
      | _ -> (* assert false *) Lwt.return_unit)
  | (Request.Request_validation _, Validation_error errs) -> (
      let () = Worker_metrics.update metrics.validation_worker_metrics st in
      Prometheus.Counter.inc_one metrics.validation_errors_count ;
      match Request.view request with
      | Validation v ->
          Worker.log_event w (Event.Validation_failure (v, st, errs))
      | _ -> (* assert false *) Lwt.return_unit)
  | (Request.Request_preapplication _, Preapplied _) -> (
      Prometheus.Counter.inc_one metrics.preapplied_blocks_count ;
      match Request.view request with
      | Preapplication v ->
          Worker.log_event w (Event.Preapplication_success (v, st))
      | _ -> (* assert false *) Lwt.return_unit)
  | (Request.Request_preapplication _, Preapplication_error errs) -> (
      Prometheus.Counter.inc_one metrics.preapplication_errors_count ;
      match Request.view request with
      | Preapplication v ->
          Worker.log_event w (Event.Preapplication_failure (v, st, errs))
      | _ -> (* assert false *) Lwt.return_unit)
  | (Request.Request_validation _, Validation_error_after_precheck errs) -> (
      let () = Worker_metrics.update metrics.validation_worker_metrics st in
      Prometheus.Counter.inc_one metrics.validation_errors_after_precheck_count ;
      match Request.view request with
      | Validation v ->
          Worker.log_event
            w
            (Event.Validation_failure_after_precheck (v, st, errs))
      | _ -> (* assert false *) Lwt.return_unit)
  | (Request.Request_validation _, Precheck_failed errs) -> (
      let () = Worker_metrics.update metrics.validation_worker_metrics st in
      Prometheus.Counter.inc_one metrics.precheck_failed_count ;
      match Request.view request with
      | Validation v ->
          Worker.log_event w (Event.Precheck_failure (v, st, errs))
      | _ -> (* assert false *) Lwt.return_unit)
  | _ -> (* assert false *) Lwt.return_unit

let on_close w =
  let bv = Worker.state w in
  Block_validator_process.close bv.validation_process

let table = Worker.create_table Queue

let create limits db validation_process ~start_testchain =
  let module Handlers = struct
    type self = t

    let on_launch = on_launch

    let on_request = on_request

    let on_close = on_close

    let on_error = on_error

    let on_completion = on_completion

    let on_no_request _ = return_unit
  end in
  Worker.launch
    table
    ()
    (limits, start_testchain, db, validation_process)
    (module Handlers)

let shutdown = Worker.shutdown

type block_validity =
  | Valid
  | Invalid_after_precheck of error trace
  | Invalid of error trace

let validate w ?canceler ?peer ?(notify_new_block = fun _ -> ())
    ?(precheck_and_notify = false) chain_db hash (header : Block_header.t)
    operations : block_validity Lwt.t =
  let chain_store = Distributed_db.chain_store chain_db in
  is_already_validated chain_store hash >>= function
  | true ->
      Worker.log_event w (Previously_validated hash) >>= fun () ->
      Lwt.return Valid
  | false -> (
      (let hashes = List.map (List.map Operation.hash) operations in
       let computed_hash =
         Operation_list_list_hash.compute
           (List.map Operation_list_hash.compute hashes)
       in
       fail_when
         (Operation_list_list_hash.compare
            computed_hash
            header.shell.operations_hash
         <> 0)
         (Inconsistent_operations_hash
            {
              block = hash;
              expected = header.shell.operations_hash;
              found = computed_hash;
            })
       >>=? fun () ->
       check_chain_liveness chain_db hash header >>=? fun () ->
       Worker.Queue.push_request_and_wait
         w
         (Request_validation
            {
              chain_db;
              notify_new_block;
              canceler;
              peer;
              hash;
              header;
              operations;
              precheck_and_notify;
            }))
      >>= function
      | Ok (Validated | Already_commited | Outdated_block) -> Lwt.return Valid
      | Ok (Validation_error_after_precheck errs) ->
          Lwt.return (Invalid_after_precheck errs)
      | Ok (Precheck_failed errs) | Ok (Validation_error errs) | Error errs ->
          Lwt.return (Invalid errs)
      | _ -> assert false)

let preapply w ?canceler chain_store ~predecessor ~timestamp ~protocol_data
    operations =
  Worker.Queue.push_request_and_wait
    w
    (Request_preapplication
       {
         chain_store;
         canceler;
         predecessor;
         timestamp;
         protocol_data;
         operations;
       })
  >>= function
  | Ok (Preapplied res) -> return res
  | Ok (Preapplication_error errs) | Error errs -> Lwt.return_error errs
  | _ -> failwith "unexpected result"

let fetch_and_compile_protocol w =
  let bv = Worker.state w in
  Protocol_validator.fetch_and_compile_protocol bv.protocol_validator

let status = Worker.status

let running_worker () =
  match Worker.list table with
  | [(_, single)] -> single
  | [] -> raise Not_found
  | _ :: _ :: _ ->
      (* NOTE: names of workers must be unique, [Name.t = unit] which has only
         one inhabitant. *)
      assert false

let pending_requests t = Worker.Queue.pending_requests t

let current_request t = Worker.current_request t
