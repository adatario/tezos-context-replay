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

(**
   Amendments and proposals.

   Only delegates having the minimal required stake take part in the amendment
   procedure.  It works as follows:

   - Proposal period: delegates can submit protocol amendment
   proposals using the proposal operation. At the end of a proposal
   period, the proposal with most supporters is selected and we move
   to an exploration period. If there are no proposals, or a tie
   between proposals, a new proposal period starts.

   - Exploration period: delegates can cast votes to test or not the
   winning proposal using the ballot operation.  At the end of an
   exploration period if participation reaches the quorum and the
   proposal has a supermajority in favor, we proceed to a cooldown
   period. Otherwise we go back to a proposal period.  In any case, if
   there is enough participation the quorum is updated.

   - Cooldown period: business as usual for the main chain. This
   period is only a time gap between exploration and promotion
   periods intended to provide the community with extra time to
   continue testing the new protocol proposal, and start adapting
   their infrastructure in advance.  At the end of the Cooldown
   period we move to the Promotion period.

   - Promotion period: delegates can cast votes to promote or not the
   proposal using the ballot operation.  At the end of a promotion
   period if participation reaches the quorum and the proposal has a
   supermajority in favor, we move to an adoption period. Otherwise we
   go back to a proposal period.  In any case, if there is enough
   participation the quorum is updated.

   - Adoption period: At the end of an adoption period, the proposal
   is activated as the new protocol.

   The current protocol parameters are documented in
   src/proto_alpha/lib_parameters/default_parameters.ml

   In practice, the real constants used are defined in the
   migration code. In src/proto_alpha/lib_protocol/init_storage.ml,
   function [prepare_first_block] introduces new constants and
   redefines the existing ones.
*)

open Alpha_context

(** If at the end of a voting period, moves to the next one following
    the state machine of the amendment procedure. *)
val may_start_new_voting_period : context -> context tzresult Lwt.t

(** {2 Validation and application of voting operations}

    There are two kinds of voting operations:

    - Proposals: A delegate submits a list of protocol amendment
      proposals. This operation is only accepted during a Proposal period
      (see above).

    - Ballot: A delegate casts a vote for/against the current proposal
      (or pass). This operation is only accepted during an Exploration
      or Promotion period (see above). *)

(** A state containing a summary of previously validated voting
    operations. It should be maintained in memory during the validation
    of a block, or until a change of head block in mempool mode. It is
    used to check for conflicts between an operation to validate and
    the already validated operations of the current block/mempool. *)
module Validation_state : sig
  (** A state as described right above. *)
  type t

  (** The empty state (for the initialization of a new block or mempool). *)
  val empty : t
end

(** Check that a Proposals operation can be safely applied.

    @return [Error Wrong_voting_period_index] if the operation's
    period and the [context]'s current period do not have the same
    index.

    @return [Error Empty_proposals] if the list of proposals is empty.

    @return [Error Proposals_contain_duplicate] if the list of
    proposals contains a duplicate element.

    @return [Error Wrong_voting_period_kind] if the voting period is
    not of the Proposal kind.

    @return [Error Source_not_in_vote_listings] if the source is not
    in the vote listings.

    @return [Error Too_many_proposals] if the operation would make the
    source's total number of proposals exceed
    {!Constants.recorded_proposal_count_for_delegate}.

    @return [Error Already_proposed] if one of the proposals has
    already been proposed by the source.

    @return [Error Conflict_too_many_proposals] if the total count of
    proposals submitted by the source in previous blocks, in previously
    validated operations of the current block/mempool, and in the
    operation to validate, exceeds
    {!Constants.max_proposals_per_delegate}.

    @return [Error Conflict_already_proposed] if one of the
    operation's proposals has already been submitted by the source in
    the current block/mempool.

    @return [Error Conflicting_dictator_proposals] if a testnet
    dictator Proposals operation has already been validated in the
    current block/mempool.

    @return [Error Testnet_dictator_multiple_proposals] if the source
    is a testnet dictator and the operation contains more than one
    proposal.

    @return [Error Testnet_dictator_conflicting_operation] if the
    source is a testnet dictator and the current block or mempool
    already contains any validated voting operation.

    @return [Error Operation.Missing_signature] or [Error
    Operation.Invalid_signature] if the operation is unsigned or
    incorrectly signed. *)
val validate_proposals :
  context ->
  Chain_id.t ->
  Validation_state.t ->
  should_check_signature:bool ->
  Operation_hash.t ->
  Kind.proposals operation ->
  Validation_state.t tzresult Lwt.t

(** Update the [context] with the effects of a Proposals operation:

    - Its proposals are added to the source's recorded proposals.

    - The recorded proposal count of the source is increased by the
      number of proposals in the operation.

    Note that a Proposals operation from a testnet dictator (which may
    be set up when a test chain is initialized) has completely
    different effects:

    - If the operation contains no proposal, then the current voting
      period is immediately and forcibly set to a Proposal period.

    - If the operation contains exactly one proposal, then the current
      voting period is immediately and forcibly set to an Adoption period
      for this proposal.

    {!validate_proposals} must have been called beforehand, and is
    responsible for ensuring that [apply_proposals] cannot fail. *)
val apply_proposals :
  context ->
  Chain_id.t ->
  Kind.proposals contents ->
  (context * Kind.proposals Apply_results.contents_result_list) tzresult Lwt.t

(** Check that a Ballot operation can be safely applied.

    @return [Error Conflicting_ballot] if the source has already
    submitted a ballot in the current block/mempool.

    @return [Error Conflicting_dictator_proposals] if the current
    block/mempool already contains a validated testnet dictator
    Proposals operation.

    @return [Error Wrong_voting_period_index] if the operation's
    period and the [context]'s current period do not have the same
    index.

    @return [Error Wrong_voting_period_kind] if the voting period is
    not of the Exploration or Promotion kind.

    @return [Error Ballot_for_wrong_proposal] if the operation's
    proposal is different from the [context]'s current proposal.

    @return [Error Already_submitted_a_ballot] if the source has
    already voted.

    @return [Error Source_not_in_vote_listings] if the source is not
    in the vote listings.

    @return [Error Operation.Missing_signature] or [Error
    Operation.Invalid_signature] if the operation is unsigned or
    incorrectly signed. *)
val validate_ballot :
  context ->
  Chain_id.t ->
  Validation_state.t ->
  should_check_signature:bool ->
  Operation_hash.t ->
  Kind.ballot operation ->
  Validation_state.t tzresult Lwt.t

(** Update the [context] with the effects of a Ballot operation:

    The couple (source of the operation, submitted ballot) is recorded.

    {!validate_ballot} must have been called beforehand, and is
    responsible for ensuring that [apply_ballot] cannot fail. *)
val apply_ballot :
  context ->
  Kind.ballot contents ->
  (context * Kind.ballot Apply_results.contents_result_list) tzresult Lwt.t
