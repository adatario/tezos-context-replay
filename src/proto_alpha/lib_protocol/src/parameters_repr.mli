(**************************************************************************)
(*                                                                        *)
(*    Copyright (c) 2014 - 2018.                                          *)
(*    Dynamic Ledger Solutions, Inc. <contact@tezos.com>                  *)
(*                                                                        *)
(*    All rights reserved. No warranty, explicit or implicit, provided.   *)
(*                                                                        *)
(**************************************************************************)

type bootstrap_account = {
  public_key_hash : Signature.Public_key_hash.t ;
  public_key : Signature.Public_key.t option ;
  amount : Tez_repr.t ;
}

type bootstrap_contract = {
  delegate : Signature.Public_key_hash.t ;
  amount : Tez_repr.t ;
  script : Script_repr.t ;
}

type t = {
  bootstrap_accounts : bootstrap_account list ;
  bootstrap_contracts : bootstrap_contract list ;
  commitments : Commitment_repr.t list ;
  constants : Constants_repr.parametric ;
  security_deposit_ramp_up_cycles : int option ;
  no_reward_cycles : int option ;
}

val encoding: t Data_encoding.t
val constants_encoding: Constants_repr.parametric Data_encoding.t
