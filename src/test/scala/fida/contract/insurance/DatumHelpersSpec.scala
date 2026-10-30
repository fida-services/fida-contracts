package fida.contract.insurance

import fida.contract.insurance.InsurancePolicyDatum.*
import fida.contract.insurance.PiggyBankDatum.*
import fida.contract.testkit.Generators.*
import org.scalatest.funsuite.AnyFunSuite
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks
import scalus.cardano.onchain.plutus.prelude.{List as PList, Option}
import scalus.uplc.builtin.Data.toData

/** The datum update helpers of `Fida.Contract.Insurance.Datum`: each changes one field of one
  * constructor and returns `None` for the other constructors.
  */
class DatumHelpersSpec extends AnyFunSuite with ScalaCheckPropertyChecks {

    /** `f` of the policy info, `None` for the other policy datums. */
    private def onInsuranceInfo(datum: InsurancePolicyDatum)(
        f: InsuranceInfo => InsuranceInfo
    ): Option[InsurancePolicyDatum] = datum match
        case info: InsuranceInfo => Option.Some(f(info))
        case _                   => Option.None

    test("updatePolicyState sets the state of policy info") {
        forAll(genPolicyDatum, genState) { (datum, state) =>
            val expected = onInsuranceInfo(datum)(_.copy(iInfoState = state))
            assert(updatePolicyState(datum, state) == expected)
            assert(untypedUpdatePolicyState(datum, state) == expected.map(_.toData))
        }
    }

    test("completeFunding puts policy info on risk from a start date") {
        forAll(genPolicyDatum, genInteger) { (datum, start) =>
            assert(
              completeFunding(datum, start) == onInsuranceInfo(datum)(
                _.copy(
                  iInfoState = InsurancePolicyState.OnRisk,
                  iInfoStartDate = Option.Some(start)
                )
              )
            )
        }
    }

    test("updateClaim, untypedUpdateClaim and untypedUnsetClaim set or unset the claim") {
        forAll(genPolicyDatum, genOption(genClaim)) { (datum, claim) =>
            assert(updateClaim(datum, claim) == onInsuranceInfo(datum)(_.copy(iInfoClaim = claim)))
        }
        forAll(genPolicyDatum, genClaim) { (datum, claim) =>
            assert(
              untypedUpdateClaim(datum, claim) ==
                  onInsuranceInfo(datum)(_.copy(iInfoClaim = Option.Some(claim))).map(_.toData)
            )
            assert(
              untypedUnsetClaim(datum) ==
                  onInsuranceInfo(datum)(_.copy(iInfoClaim = Option.None)).map(_.toData)
            )
        }
    }

    /** `f` of the premium, `None` for a fida card. */
    private def onPremium(datum: PiggyBankDatum)(
        f: PBankPremium => PiggyBankDatum
    ): Option[PiggyBankDatum] = datum match
        case premium: PBankPremium => Option.Some(f(premium))
        case _                     => Option.None

    /** `f` of the fida card, `None` for a premium. */
    private def onFidaCard(datum: PiggyBankDatum)(
        f: PBankFidaCard => PiggyBankDatum
    ): Option[PiggyBankDatum] = datum match
        case card: PBankFidaCard => Option.Some(f(card))
        case _                   => Option.None

    test("updatePiggyBankRefund sets the refund of a premium") {
        forAll(genInteger, genPiggyBankDatum) { (refund, datum) =>
            val expected = onPremium(datum)(_.copy(pbankPremiumRefund = refund))
            assert(updatePiggyBankRefund(refund, datum) == expected)
            assert(untypedUpdatePiggyBankRefund(refund, datum) == expected.map(_.toData))
        }
    }

    test("addPiggyBankPaidClaim records a paid claim first in a fida card's list") {
        forAll(genBytes, genPiggyBankDatum) { (claimId, datum) =>
            assert(
              addPiggyBankPaidClaim(claimId, datum) == onFidaCard(datum)(card =>
                  card.copy(pbfcPaidClaims = PList.Cons(claimId, card.pbfcPaidClaims))
              )
            )
        }
    }

    test("setFidaCardSold and setFidaCardUnsold set whether a fida card is sold") {
        forAll(genPiggyBankDatum) { datum =>
            val sold = onFidaCard(datum)(_.copy(pbfcIsSold = true))
            val unsold = onFidaCard(datum)(_.copy(pbfcIsSold = false))
            assert(setFidaCardSold(datum) == sold)
            assert(untypedSetFidaCardSold(datum) == sold.map(_.toData))
            assert(setFidaCardUnsold(datum) == unsold)
            assert(updatePiggyBankSoldFidaCardStatus(true, datum) == sold)
            assert(updatePiggyBankSoldFidaCardStatus(false, datum) == unsold)
        }
    }
}
