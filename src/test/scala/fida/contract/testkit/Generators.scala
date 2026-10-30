package fida.contract.testkit

import fida.contract.insurance.*
import fida.contract.insurance.InsuranceAuthority.*
import fida.contract.insurance.InsurancePolicyDatum.*
import fida.contract.insurance.InsurancePolicyRedeemer.*
import fida.contract.insurance.PiggyBankDatum.*
import fida.contract.insurance.PolicyFundingRedeemer.*
import fida.contract.insurance.PolicyOnRiskRedeemer.*
import org.scalacheck.{Arbitrary, Gen}
import scalus.cardano.onchain.plutus.prelude.{List as PList, Option as POption}
import scalus.cardano.onchain.plutus.v1.{Address, Credential, PubKeyHash, StakingCredential}
import scalus.uplc.builtin.ByteString

/** ScalaCheck generators of the contracts' on-chain values, covering every constructor. */
object Generators {

    /** Small, `Long`-sized and arbitrary-precision integers, negative ones included. */
    val genInteger: Gen[BigInt] = Gen.oneOf(
      Gen.choose(-1000L, 1000L).map(BigInt(_)),
      Arbitrary.arbitrary[Long].map(BigInt(_)),
      Arbitrary.arbitrary[BigInt]
    )

    val genBytes: Gen[ByteString] =
        Gen.choose(0, 40)
            .flatMap(Gen.listOfN(_, Arbitrary.arbitrary[Byte]))
            .map(bs => ByteString.unsafeFromArray(bs.toArray))

    /** A 28-byte hash (key, script or currency symbol). */
    val genHash: Gen[ByteString] =
        Gen.listOfN(28, Arbitrary.arbitrary[Byte]).map(bs => ByteString.unsafeFromArray(bs.toArray))

    val genPkh: Gen[PubKeyHash] = genHash.map(PubKeyHash(_))

    def genList[A](gen: Gen[A]): Gen[PList[A]] =
        Gen.choose(0, 5).flatMap(Gen.listOfN(_, gen)).map(PList.from)

    def genOption[A](gen: Gen[A]): Gen[POption[A]] =
        Gen.option(gen).map(_.fold(POption.None)(POption.Some(_)))

    val genCredential: Gen[Credential] = Gen.oneOf(
      genPkh.map(Credential.PubKeyCredential(_)),
      genHash.map(Credential.ScriptCredential(_))
    )

    val genStakingCredential: Gen[StakingCredential] = Gen.oneOf(
      genCredential.map(StakingCredential.StakingHash(_)),
      Gen.zip(genInteger, genInteger, genInteger).map(StakingCredential.StakingPtr(_, _, _))
    )

    val genAddress: Gen[Address] =
        Gen.zip(genCredential, genOption(genStakingCredential)).map(Address(_, _))

    val genState: Gen[InsurancePolicyState] = Gen.oneOf(InsurancePolicyState.values.toSeq)

    val genAuthority: Gen[InsuranceAuthority] = Gen.oneOf(
      genPkh.map(SingleSign(_)),
      genList(genPkh).map(AtLeastOneSign(_)),
      genList(genPkh).map(AllMustSign(_)),
      genList(genPkh).map(MajorityMustSign(_))
    )

    val genInstallments: Gen[InstallmentsInfo] =
        Gen.zip(genList(genInteger), genInteger).map(InstallmentsInfo(_, _))

    val genClaim: Gen[ClaimInfo] = for
        amount <- genInteger
        date <- genInteger
        reason <- genBytes
        accepted <- Gen.oneOf(true, false)
        id <- genBytes
    yield ClaimInfo(amount, date, reason, accepted, id)

    val genInsuranceInfo: Gen[InsuranceInfo] = for
        ints <- Gen.listOfN(10, genInteger)
        installments <- genInstallments
        holder <- genPkh
        authority <- genAuthority
        state <- genState
        start <- genOption(genInteger)
        claim <- genOption(genClaim)
    yield (InsuranceInfo(
      ints(0),
      ints(1),
      ints(2),
      ints(3),
      installments,
      ints(4),
      holder,
      authority,
      state,
      ints(5),
      ints(6),
      ints(7),
      ints(8),
      start,
      claim
    ): InsuranceInfo)

    val genPolicyDatum: Gen[InsurancePolicyDatum] = Gen.oneOf(
      genInsuranceInfo,
      Gen.zip(genInteger, genList(genAddress)).map(PremiumPaymentInfo(_, _)),
      Gen.const(PolicyClaimPayment)
    )

    val genPiggyBankDatum: Gen[PiggyBankDatum] = Gen.oneOf(
      Gen.zip(genInteger, genInteger).map(PBankPremium(_, _)),
      for
          sold <- Gen.oneOf(true, false)
          id <- genBytes
          value <- genInteger
          paid <- genList(genBytes)
      yield PBankFidaCard(sold, FidaCardId(id), value, paid)
    )

    val genPolicyRedeemer: Gen[InsurancePolicyRedeemer] = Gen.oneOf(
      Gen.oneOf(PolicyInitiatedRedeemer.values.toSeq).map(PolicyInitiated(_)),
      Gen.oneOf(
        Gen.const(PolicyFundingCancel),
        genInteger.map(PolicyFundingFundingComplete(_))
      ).map(PolicyFunding(_)),
      Gen.oneOf(
        genClaim.map(PolicyOnRiskCreateClaim(_)),
        Gen.oneOf(
          PolicyOnRiskCloseClaim,
          PolicyOnRiskAcceptClaim,
          PolicyOnRiskFinalizeClaim,
          PolicyOnRiskExpireClaim,
          PolicyOnRiskFailClaim,
          PolicyOnRiskCancel,
          PolicyOnRiskClaimPayment
        )
      ).map(PolicyOnRisk(_)),
      Gen.const(PolicyExpire)
    )
}
