package fida.contract.insurance

import fida.contract.Utils.{before, check, traceError, traceIfFalse, untypedOutputDatum}
import fida.contract.insurance.InsurancePolicyDatum.{
    InsuranceInfo,
    PolicyClaimPayment,
    PremiumPaymentInfo,
    untypedUpdatePolicyState
}
import fida.contract.insurance.InsurancePolicyRedeemer.*
import fida.contract.insurance.InsurancePolicyState.{Cancelled, Expired, Funding, Initiated, OnRisk}
import fida.contract.insurance.PolicyFundingRedeemer.{
    PolicyFundingCancel,
    PolicyFundingFundingComplete
}
import fida.contract.insurance.PolicyInitiatedRedeemer.{
    PolicyInitiatedCancel,
    PolicyInitiatedPayPremium
}
import fida.contract.insurance.PolicyOnRiskRedeemer.*
import fida.contract.insurance.lifecycle.{
    CancelledLifecycle,
    FundingLifecycle,
    InitiatedLifecycle,
    OnRiskLifecycle
}
import scalus.compiler.Compile
import scalus.uplc.builtin.{BuiltinList, Builtins, Data}
import scalus.uplc.builtin.Builtins.{headList, tailList, unBData, unConstrData, unIData, unListData}
import scalus.uplc.builtin.Data.to
import scalus.cardano.onchain.plutus.v1.{Address, Credential, PubKeyHash, StakingCredential}
import scalus.cardano.onchain.plutus.v2.ScriptContext
import scalus.cardano.onchain.plutus.prelude.*

/** Insurance policy validator (port of `Fida.Contract.Insurance`).
  *
  * Parameter: the [[InsuranceId]]. Dispatches on the redeemer and datum in the same order as the
  * Haskell pattern match:
  *   1. `PolicyInitiated r` (any datum) is handled by [[InitiatedLifecycle]];
  *   2. any other redeemer on an `InsuranceInfo` datum in state `Cancelled` is handled by
  *      [[CancelledLifecycle]] (always rejected);
  *   3. `PolicyFunding r` by [[FundingLifecycle]], `PolicyOnRisk r` by [[OnRiskLifecycle]];
  *   4. `PolicyExpire` on an `InsuranceInfo` datum moves a policy to `Expired`.
  *
  * On-chain errors:
  *   - `ERR-MAIN-0`: illegal action for the main validator
  *   - `ERR-MAIN-1`: expiry requested for a policy that is neither `Funding` nor `OnRisk` with a
  *     start date
  *   - `ERR-MAIN-2`: the policy has not expired yet
  *   - `ERR-MAIN-3`: the policy info output datum is not the input datum in state `Expired`
  */
@Compile
object InsurancePolicyValidator {

    def validate(
        iid: InsuranceId,
        datum: InsurancePolicyDatum,
        redeemer: InsurancePolicyRedeemer,
        ctx: ScriptContext
    ): Boolean =
        redeemer match
            case PolicyInitiated(r) => InitiatedLifecycle.validate(iid, datum, r, ctx)
            case _                  =>
                if isCancelled(datum) then CancelledLifecycle.validate(iid, datum, redeemer, ctx)
                else
                    redeemer match
                        case PolicyFunding(r) => FundingLifecycle.validate(iid, datum, r, ctx)
                        case PolicyOnRisk(r)  => OnRiskLifecycle.validate(iid, datum, r, ctx)
                        case PolicyExpire     => validateExpire(iid, datum, ctx)
                        case _                => Builtins.trace("ERR-MAIN-0")(false)

    private def isCancelled(datum: InsurancePolicyDatum): Boolean = datum match
        case InsuranceInfo(_, _, _, _, _, _, _, _, state, _, _, _, _, _, _) => state === Cancelled
        case _                                                              => false

    private def validateExpire(
        iid: InsuranceId,
        datum: InsurancePolicyDatum,
        ctx: ScriptContext
    ): Boolean =
        datum match
            case InsuranceInfo(
                  _,
                  _,
                  _,
                  _,
                  _,
                  insurancePeriod,
                  _,
                  _,
                  state,
                  fundingDeadline,
                  _,
                  _,
                  _,
                  startDate,
                  _
                ) =>
                val expireDate =
                    if state === Funding then fundingDeadline
                    else if state === OnRisk then
                        startDate match
                            case Option.Some(start) => start + insurancePeriod
                            case Option.None        => traceError("ERR-MAIN-1")
                    else traceError("ERR-MAIN-1")
                traceIfFalse("ERR-MAIN-2", before(expireDate, ctx.txInfo.validRange))
                && traceIfFalse(
                  "ERR-MAIN-3",
                  untypedOutputDatum(iid.cs, ctx, Tokens.policyInfoTokenName)
                      === untypedUpdatePolicyState(datum, Expired)
                )
            case _ => Builtins.trace("ERR-MAIN-0")(false)

    /** Entry point, the equivalent of the Haskell `mkInsurancePolicyValidatorUntyped`.
      *
      * Like PlutusTx `unsafeFromBuiltinData` in the Haskell wrapper, decodes the parameter, then
      * the datum, then the redeemer strictly before validating: malformed data fails the script, an
      * unknown constructor fails with `PT1`, and extra constructor fields are ignored. The datum
      * and redeemer are rebuilt from the decoded values, so re-encoding them (e.g. the expected
      * continuing datum) gives the canonical encoding, as `toBuiltinData` does in the Haskell code.
      * The script context comes from the ledger and is well-formed. A false result fails with `PT5`
      * (PlutusTx `check`).
      */
    def untyped(iid: Data, datum: Data, redeemer: Data, ctx: Data): Unit =
        val insuranceId = InsuranceId(unBData(iid))
        val decodedDatum = decodeInsurancePolicyDatum(datum)
        val decodedRedeemer = decodeInsurancePolicyRedeemer(redeemer)
        check(validate(insuranceId, decodedDatum, decodedRedeemer, ctx.to[ScriptContext]))

    // ---------------------------------------------------------------------------------------------
    // Strict decoding: PlutusTx `unsafeFromBuiltinData` as generated by `makeIsDataIndexed` (and the
    // plutus-tx / plutus-ledger-api instances for `Bool`, `Maybe`, lists and addresses). Each helper
    // takes only the `Data` it decodes (see the Scalus note in docs/MIGRATION.md).

    private def decodeInsurancePolicyDatum(d: Data): InsurancePolicyDatum =
        val constr = unConstrData(d)
        val tag = constr.fst
        val f0 = constr.snd
        if tag === BigInt(0) then
            val f1 = tailList(f0)
            val f2 = tailList(f1)
            val f3 = tailList(f2)
            val f4 = tailList(f3)
            val f5 = tailList(f4)
            val f6 = tailList(f5)
            val f7 = tailList(f6)
            val f8 = tailList(f7)
            val f9 = tailList(f8)
            val f10 = tailList(f9)
            val f11 = tailList(f10)
            val f12 = tailList(f11)
            val f13 = tailList(f12)
            val f14 = tailList(f13)
            InsuranceInfo(
              iInfoCollateralAmount = unIData(headList(f0)),
              iInfoFidaCardValue = unIData(headList(f1)),
              iInfoFidaCardNumber = unIData(headList(f2)),
              iInfoPremiumAmount = unIData(headList(f3)),
              iInfoInstallments = decodeInstallmentsInfo(headList(f4)),
              iInfoInsurancePeriod = unIData(headList(f5)),
              iInfoPolicyHolder = PubKeyHash(unBData(headList(f6))),
              iInfoPolicyAuthority = decodeAuthority(headList(f7)),
              iInfoState = decodePolicyState(headList(f8)),
              iInfoFundingDeadline = unIData(headList(f9)),
              iInfoTotalClaimsAcceptedAmount = unIData(headList(f10)),
              iInfoClaimTimeToLive = unIData(headList(f11)),
              iInfoClaimTimeToPay = unIData(headList(f12)),
              iInfoStartDate = decodeMaybeInteger(headList(f13)),
              iInfoClaim = decodeMaybeClaimInfo(headList(f14))
            )
        else if tag === BigInt(1) then
            PremiumPaymentInfo(
              unIData(headList(f0)),
              decodeAddresses(unListData(headList(tailList(f0))))
            )
        else if tag === BigInt(2) then PolicyClaimPayment
        else traceError("PT1")

    private def decodeInstallmentsInfo(d: Data): InstallmentsInfo =
        val constr = unConstrData(d)
        val f0 = constr.snd
        if constr.fst === BigInt(0) then
            InstallmentsInfo(
              decodeIntegers(unListData(headList(f0))),
              unIData(headList(tailList(f0)))
            )
        else traceError("PT1")

    private def decodeAuthority(d: Data): InsuranceAuthority =
        val constr = unConstrData(d)
        val tag = constr.fst
        val field = headList(constr.snd)
        if tag === BigInt(0) then InsuranceAuthority.SingleSign(PubKeyHash(unBData(field)))
        else if tag === BigInt(1) then
            InsuranceAuthority.AtLeastOneSign(decodePubKeyHashes(unListData(field)))
        else if tag === BigInt(2) then
            InsuranceAuthority.AllMustSign(decodePubKeyHashes(unListData(field)))
        else if tag === BigInt(3) then
            InsuranceAuthority.MajorityMustSign(decodePubKeyHashes(unListData(field)))
        else traceError("PT1")

    private def decodePolicyState(d: Data): InsurancePolicyState =
        val tag = unConstrData(d).fst
        if tag === BigInt(0) then Initiated
        else if tag === BigInt(1) then Funding
        else if tag === BigInt(2) then OnRisk
        else if tag === BigInt(3) then Cancelled
        else if tag === BigInt(4) then Expired
        else traceError("PT1")

    private def decodeMaybeInteger(d: Data): Option[BigInt] =
        val constr = unConstrData(d)
        val tag = constr.fst
        if tag === BigInt(0) then Option.Some(unIData(headList(constr.snd)))
        else if tag === BigInt(1) then Option.None
        else traceError("PT1")

    private def decodeMaybeClaimInfo(d: Data): Option[ClaimInfo] =
        val constr = unConstrData(d)
        val tag = constr.fst
        if tag === BigInt(0) then Option.Some(decodeClaimInfo(headList(constr.snd)))
        else if tag === BigInt(1) then Option.None
        else traceError("PT1")

    private def decodeClaimInfo(d: Data): ClaimInfo =
        val constr = unConstrData(d)
        val f0 = constr.snd
        if constr.fst === BigInt(0) then
            val f1 = tailList(f0)
            val f2 = tailList(f1)
            val f3 = tailList(f2)
            val f4 = tailList(f3)
            ClaimInfo(
              claimAmount = unIData(headList(f0)),
              claimDate = unIData(headList(f1)),
              claimReason = unBData(headList(f2)),
              claimAccepted = decodeBool(headList(f3)),
              claimId = unBData(headList(f4))
            )
        else traceError("PT1")

    private def decodeBool(d: Data): Boolean =
        val tag = unConstrData(d).fst
        if tag === BigInt(0) then false
        else if tag === BigInt(1) then true
        else traceError("PT1")

    private def decodeIntegers(xs: BuiltinList[Data]): List[BigInt] =
        if Builtins.nullList(xs) then List.Nil
        else List.Cons(unIData(headList(xs)), decodeIntegers(tailList(xs)))

    private def decodePubKeyHashes(xs: BuiltinList[Data]): List[PubKeyHash] =
        if Builtins.nullList(xs) then List.Nil
        else List.Cons(PubKeyHash(unBData(headList(xs))), decodePubKeyHashes(tailList(xs)))

    private def decodeAddresses(xs: BuiltinList[Data]): List[Address] =
        if Builtins.nullList(xs) then List.Nil
        else List.Cons(decodeAddress(headList(xs)), decodeAddresses(tailList(xs)))

    private def decodeAddress(d: Data): Address =
        val constr = unConstrData(d)
        val f0 = constr.snd
        if constr.fst === BigInt(0) then
            Address(
              decodeCredential(headList(f0)),
              decodeMaybeStakingCredential(headList(tailList(f0)))
            )
        else traceError("PT1")

    private def decodeCredential(d: Data): Credential =
        val constr = unConstrData(d)
        val tag = constr.fst
        val hash = unBData(headList(constr.snd))
        if tag === BigInt(0) then Credential.PubKeyCredential(PubKeyHash(hash))
        else if tag === BigInt(1) then Credential.ScriptCredential(hash)
        else traceError("PT1")

    private def decodeMaybeStakingCredential(d: Data): Option[StakingCredential] =
        val constr = unConstrData(d)
        val tag = constr.fst
        if tag === BigInt(0) then Option.Some(decodeStakingCredential(headList(constr.snd)))
        else if tag === BigInt(1) then Option.None
        else traceError("PT1")

    private def decodeStakingCredential(d: Data): StakingCredential =
        val constr = unConstrData(d)
        val tag = constr.fst
        val f0 = constr.snd
        if tag === BigInt(0) then StakingCredential.StakingHash(decodeCredential(headList(f0)))
        else if tag === BigInt(1) then
            val f1 = tailList(f0)
            StakingCredential.StakingPtr(
              unIData(headList(f0)),
              unIData(headList(f1)),
              unIData(headList(tailList(f1)))
            )
        else traceError("PT1")

    private def decodeInsurancePolicyRedeemer(d: Data): InsurancePolicyRedeemer =
        val constr = unConstrData(d)
        val tag = constr.fst
        val fields = constr.snd
        if tag === BigInt(0) then PolicyInitiated(decodeInitiatedRedeemer(headList(fields)))
        else if tag === BigInt(1) then PolicyFunding(decodeFundingRedeemer(headList(fields)))
        else if tag === BigInt(2) then PolicyOnRisk(decodeOnRiskRedeemer(headList(fields)))
        else if tag === BigInt(3) then PolicyExpire
        else traceError("PT1")

    private def decodeInitiatedRedeemer(d: Data): PolicyInitiatedRedeemer =
        val tag = unConstrData(d).fst
        if tag === BigInt(0) then PolicyInitiatedCancel
        else if tag === BigInt(1) then PolicyInitiatedPayPremium
        else traceError("PT1")

    private def decodeFundingRedeemer(d: Data): PolicyFundingRedeemer =
        val constr = unConstrData(d)
        val tag = constr.fst
        if tag === BigInt(0) then PolicyFundingCancel
        else if tag === BigInt(1) then PolicyFundingFundingComplete(unIData(headList(constr.snd)))
        else traceError("PT1")

    private def decodeOnRiskRedeemer(d: Data): PolicyOnRiskRedeemer =
        val constr = unConstrData(d)
        val tag = constr.fst
        if tag === BigInt(0) then PolicyOnRiskCreateClaim(decodeClaimInfo(headList(constr.snd)))
        else if tag === BigInt(1) then PolicyOnRiskCloseClaim
        else if tag === BigInt(2) then PolicyOnRiskAcceptClaim
        else if tag === BigInt(3) then PolicyOnRiskFinalizeClaim
        else if tag === BigInt(4) then PolicyOnRiskExpireClaim
        else if tag === BigInt(5) then PolicyOnRiskFailClaim
        else if tag === BigInt(6) then PolicyOnRiskCancel
        else if tag === BigInt(7) then PolicyOnRiskClaimPayment
        else traceError("PT1")
}
