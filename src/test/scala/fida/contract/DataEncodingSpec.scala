package fida.contract

import fida.contract.insurance.*
import fida.contract.insurance.InsuranceAuthority.*
import fida.contract.insurance.InsurancePolicyDatum.*
import fida.contract.insurance.InsurancePolicyRedeemer.*
import fida.contract.insurance.PiggyBankDatum.*
import fida.contract.insurance.PiggyBankRedeemer.*
import fida.contract.insurance.PolicyFundingRedeemer.*
import fida.contract.insurance.PolicyInitiatedRedeemer.*
import fida.contract.insurance.PolicyOnRiskRedeemer.*
import org.scalacheck.{Arbitrary, Gen}
import org.scalactic.anyvals.PosInt
import org.scalatest.funsuite.AnyFunSuite
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks
import scalus.cardano.ledger.Script
import scalus.cardano.onchain.plutus.prelude.{List as PList, Option as POption}
import scalus.cardano.onchain.plutus.v1.{
    Address,
    Credential,
    PubKeyHash,
    StakingCredential,
    TxId,
    TxOutRef
}
import scalus.uplc.builtin.{ByteString, Data, FromData, ToData}
import scalus.uplc.builtin.Data.{fromData, toData}

/** The on-chain encoding of every contract type is the one of the Haskell contracts, so datums and
  * redeemers built by the existing off-chain code (and script parameters applied by CTL
  * `applyArgs`) keep working.
  *
  * Each expected `Data` is written out by hand from the Haskell definitions: `makeIsDataIndexed`
  * gives the constructor indices, a newtype is encoded as the type it wraps (`FidaCardId`,
  * `InsuranceId`, `SystemId`, `PubKeyHash`, `CurrencySymbol` and `ValidatorHash` are bare
  * bytestrings, `POSIXTime` and `DiffMilliSeconds` bare integers), `Maybe` is `Just` 0 / `Nothing`
  * 1, `Bool` is `False` 0 / `True` 1, and the plutus-ledger-api V2 types follow their own
  * `makeIsDataIndexed` declarations.
  */
class DataEncodingSpec extends AnyFunSuite with ScalaCheckPropertyChecks {

    override implicit val generatorDrivenConfig: PropertyCheckConfiguration =
        PropertyCheckConfiguration(minSuccessful = PosInt(200))

    // Data, spelled out ---------------------------------------------------------------------

    private def constr(index: Int, fields: Data*): Data = Data.Constr(index, PList.from(fields))
    private def int(n: BigInt): Data = Data.I(n)
    private def bytes(bs: ByteString): Data = Data.B(bs)
    private def list(items: Data*): Data = Data.List(PList.from(items))

    /** Haskell `Bool`. */
    private val False: Data = constr(0)
    private val True: Data = constr(1)

    /** Haskell `Maybe`. */
    private def just(d: Data): Data = constr(0, d)
    private val nothing: Data = constr(1)

    /** Haskell `()`. */
    private val unit: Data = constr(0)

    /** `value` encodes to `expected`, and decodes back from it. */
    private def assertEncoding[A: ToData: FromData](value: A, expected: Data): Unit =
        assert(value.toData == expected, s"encoding of $value")
        assert(fromData[A](expected) == value, s"decoding of $expected")

    // Sample values ---------------------------------------------------------------------------

    private def hex(s: String): ByteString = ByteString.fromHex(s)

    private val pkhA = PubKeyHash(hex("a1" * 28))
    private val pkhB = PubKeyHash(hex("b2" * 28))
    private val pkhC = PubKeyHash(hex("c3" * 28))
    private val scriptHash1 = hex("51" * 28)
    private val scriptHash2 = hex("52" * 28)
    private val currencySymbol = hex("ab" * 28)

    private val day = BigInt(86_400_000)

    private val claim = ClaimInfo(
      claimAmount = 10_000_000,
      claimDate = BigInt(1_710_159_132_000L),
      claimReason = ByteString.fromString("I'm sick!"),
      claimAccepted = true,
      claimId = hex("1234")
    )

    /** `ClaimInfo` 0: amount, date, reason, accepted, id. */
    private val claimData = constr(
      0,
      int(10_000_000),
      int(1_710_159_132_000L),
      bytes(ByteString.fromString("I'm sick!")),
      True,
      bytes(hex("1234"))
    )

    private val installments = InstallmentsInfo(PList(day * 90, day * 90, day * 91), 5_000_000)

    /** `InstallmentsInfo` 0: payment intervals, amount per installment. */
    private val installmentsData =
        constr(
          0,
          list(int(7_776_000_000L), int(7_776_000_000L), int(7_862_400_000L)),
          int(5_000_000)
        )

    // Script parameters -----------------------------------------------------------------------

    test("FidaCardId, InsuranceId and SystemId are newtypes: bare bytestrings") {
        assertEncoding(FidaCardId(ByteString.fromString("7")), bytes(ByteString.fromString("7")))
        assertEncoding(FidaCardId(ByteString.empty), bytes(ByteString.empty))
        assertEncoding(InsuranceId(currencySymbol), bytes(currencySymbol))
        assertEncoding(SystemId(currencySymbol), bytes(currencySymbol))
    }

    test("PubKeyHash (and SystemGovernance) is a bare bytestring, SystemMagic a bare integer") {
        assertEncoding(pkhA, bytes(hex("a1" * 28)))
        assertEncoding(BigInt(42), int(42))
    }

    test("TxOutRef is the plutus-ledger-api V2 one: Constr 0 [Constr 0 [B txId], I index]") {
        val txId = hex("0f" * 32)
        assertEncoding(TxOutRef(TxId(txId), 3), constr(0, constr(0, bytes(txId)), int(3)))
        assertEncoding(TxOutRef(TxId(txId), 0), constr(0, constr(0, bytes(txId)), int(0)))
    }

    test("minting policy redeemers and the investor datum are ()") {
        assertEncoding((), unit)
    }

    test("the script helpers apply parameters as CTL applyArgs would, with these encodings") {
        val ref = TxOutRef(TxId(hex("0f" * 32)), 3)
        val iid = InsuranceId(currencySymbol)
        val fcid = FidaCardId(ByteString.fromString("7"))
        def applied(compiled: scalus.uplc.CompiledPlutus[?], params: Data*): Script.PlutusV2 =
            Script.PlutusV2(params.foldLeft(compiled.program)(_ $ _))
        assert(
          Scripts.insuranceIdMintingPolicyScript(ref) ==
              applied(
                Scripts.insuranceIdMintingPolicy,
                constr(0, constr(0, bytes(hex("0f" * 32))), int(3))
              )
        )
        assert(
          Scripts.insurancePolicyScript(iid) ==
              applied(Scripts.insurancePolicyValidator, bytes(currencySymbol))
        )
        assert(
          Scripts.piggyBankScript(iid, fcid) ==
              applied(
                Scripts.piggyBankValidator,
                bytes(currencySymbol),
                bytes(ByteString.fromString("7"))
              )
        )
        assert(
          Scripts.investorScript(pkhA, SystemId(currencySymbol)) ==
              applied(Scripts.investorValidator, bytes(hex("a1" * 28)), bytes(currencySymbol))
        )
        assert(
          Scripts.systemIdMintingPolicyScript(pkhB, 42) ==
              applied(Scripts.systemIdMintingPolicy, bytes(hex("b2" * 28)), int(42))
        )
    }

    // Datum types -----------------------------------------------------------------------------

    test("InsurancePolicyState: Initiated 0, Funding 1, OnRisk 2, Cancelled 3, Expired 4") {
        import InsurancePolicyState.*
        assertEncoding(Initiated, constr(0))
        assertEncoding(Funding, constr(1))
        assertEncoding(OnRisk, constr(2))
        assertEncoding(Cancelled, constr(3))
        assertEncoding(Expired, constr(4))
    }

    test("InstallmentsInfo 0: [DiffMilliSeconds], PremiumAmount") {
        assertEncoding(installments, installmentsData)
        assertEncoding(InstallmentsInfo(PList.Nil, 0), constr(0, list(), int(0)))
    }

    test("ClaimInfo 0: amount, date, reason, accepted, id") {
        assertEncoding(claim, claimData)
        assertEncoding(
          ClaimInfo(-1, 0, ByteString.empty, false, ByteString.empty),
          constr(0, int(-1), int(0), bytes(ByteString.empty), False, bytes(ByteString.empty))
        )
    }

    test("InsuranceAuthority: SingleSign 0, AtLeastOneSign 1, AllMustSign 2, MajorityMustSign 3") {
        assertEncoding(SingleSign(pkhA), constr(0, bytes(hex("a1" * 28))))
        assertEncoding(
          AtLeastOneSign(PList(pkhA, pkhB)),
          constr(1, list(bytes(hex("a1" * 28)), bytes(hex("b2" * 28))))
        )
        assertEncoding(
          AllMustSign(PList(pkhC, pkhB, pkhA)),
          constr(2, list(bytes(hex("c3" * 28)), bytes(hex("b2" * 28)), bytes(hex("a1" * 28))))
        )
        assertEncoding(
          MajorityMustSign(PList(pkhA, pkhA, pkhB)),
          constr(3, list(bytes(hex("a1" * 28)), bytes(hex("a1" * 28)), bytes(hex("b2" * 28))))
        )
        assertEncoding(MajorityMustSign(PList.Nil), constr(3, list()))
    }

    test("InsurancePolicyDatum.InsuranceInfo 0: the 15 fields in declaration order") {
        val info: InsuranceInfo = InsuranceInfo(
          iInfoCollateralAmount = 10_000_000_000L,
          iInfoFidaCardValue = 1_000_000_000,
          iInfoFidaCardNumber = 10,
          iInfoPremiumAmount = 200_000_000,
          iInfoInstallments = installments,
          iInfoInsurancePeriod = day * 365,
          iInfoPolicyHolder = pkhA,
          iInfoPolicyAuthority = AtLeastOneSign(PList(pkhB, pkhC)),
          iInfoState = InsurancePolicyState.OnRisk,
          iInfoFundingDeadline = BigInt(1_709_899_932_000L),
          iInfoTotalClaimsAcceptedAmount = 30_000_000,
          iInfoClaimTimeToLive = 100_000,
          iInfoClaimTimeToPay = 50_000,
          iInfoStartDate = POption.Some(BigInt(1_709_295_132_000L)),
          iInfoClaim = POption.Some(claim)
        )
        val infoData = constr(
          0,
          int(10_000_000_000L), // iInfoCollateralAmount
          int(1_000_000_000), // iInfoFidaCardValue
          int(10), // iInfoFidaCardNumber
          int(200_000_000), // iInfoPremiumAmount
          installmentsData, // iInfoInstallments
          int(31_536_000_000L), // iInfoInsurancePeriod (DiffMilliSeconds)
          bytes(hex("a1" * 28)), // iInfoPolicyHolder (PubKeyHash)
          constr(1, list(bytes(hex("b2" * 28)), bytes(hex("c3" * 28)))), // iInfoPolicyAuthority
          constr(2), // iInfoState = OnRisk
          int(1_709_899_932_000L), // iInfoFundingDeadline (POSIXTime)
          int(30_000_000), // iInfoTotalClaimsAcceptedAmount
          int(100_000), // iInfoClaimTimeToLive
          int(50_000), // iInfoClaimTimeToPay
          just(int(1_709_295_132_000L)), // iInfoStartDate
          just(claimData) // iInfoClaim
        )
        assertEncoding[InsurancePolicyDatum](info, infoData)

        val initiated = info.copy(
          iInfoPolicyAuthority = SingleSign(pkhC),
          iInfoState = InsurancePolicyState.Initiated,
          iInfoStartDate = POption.None,
          iInfoClaim = POption.None
        )
        val initiatedData = constr(
          0,
          int(10_000_000_000L),
          int(1_000_000_000),
          int(10),
          int(200_000_000),
          installmentsData,
          int(31_536_000_000L),
          bytes(hex("a1" * 28)),
          constr(0, bytes(hex("c3" * 28))),
          constr(0),
          int(1_709_899_932_000L),
          int(30_000_000),
          int(100_000),
          int(50_000),
          nothing,
          nothing
        )
        assertEncoding[InsurancePolicyDatum](initiated, initiatedData)
    }

    test("InsurancePolicyDatum.PremiumPaymentInfo 1: amount, [Address] (V2 Address encoding)") {
        val piggyBanks = PList(
          Address(Credential.ScriptCredential(scriptHash1), POption.None),
          Address(
            Credential.PubKeyCredential(pkhA),
            POption.Some(StakingCredential.StakingHash(Credential.PubKeyCredential(pkhB)))
          ),
          Address(
            Credential.ScriptCredential(scriptHash2),
            POption.Some(StakingCredential.StakingHash(Credential.ScriptCredential(scriptHash1)))
          ),
          Address(
            Credential.PubKeyCredential(pkhC),
            POption.Some(StakingCredential.StakingPtr(1, 2, 3))
          )
        )
        // Address 0: credential (PubKeyCredential 0 / ScriptCredential 1), Maybe StakingCredential
        // (StakingHash 0 / StakingPtr 1)
        val piggyBanksData = list(
          constr(0, constr(1, bytes(hex("51" * 28))), nothing),
          constr(
            0,
            constr(0, bytes(hex("a1" * 28))),
            just(constr(0, constr(0, bytes(hex("b2" * 28)))))
          ),
          constr(
            0,
            constr(1, bytes(hex("52" * 28))),
            just(constr(0, constr(1, bytes(hex("51" * 28)))))
          ),
          constr(0, constr(0, bytes(hex("c3" * 28))), just(constr(1, int(1), int(2), int(3))))
        )
        assertEncoding[InsurancePolicyDatum](
          PremiumPaymentInfo(20_000_000, piggyBanks),
          constr(1, int(20_000_000), piggyBanksData)
        )
        assertEncoding[InsurancePolicyDatum](
          PremiumPaymentInfo(0, PList.Nil),
          constr(1, int(0), list())
        )
    }

    test("InsurancePolicyDatum.PolicyClaimPayment 2") {
        assertEncoding[InsurancePolicyDatum](PolicyClaimPayment, constr(2))
    }

    test(
      "PiggyBankDatum: PBankPremium 0 (init, refund), PBankFidaCard 1 (sold, id, value, paid claims)"
    ) {
        assertEncoding[PiggyBankDatum](
          PBankPremium(20_000_000, 7_500_000),
          constr(0, int(20_000_000), int(7_500_000))
        )
        assertEncoding[PiggyBankDatum](
          PBankFidaCard(
            true,
            FidaCardId(ByteString.fromString("7")),
            1_000_000_000,
            PList(hex("1234"), hex("5678"))
          ),
          constr(
            1,
            True,
            bytes(ByteString.fromString("7")),
            int(1_000_000_000),
            list(bytes(hex("1234")), bytes(hex("5678")))
          )
        )
        assertEncoding[PiggyBankDatum](
          PBankFidaCard(false, FidaCardId(hex("00")), 0, PList.Nil),
          constr(1, False, bytes(hex("00")), int(0), list())
        )
    }

    // Redeemers -------------------------------------------------------------------------------

    test("PolicyInitiatedRedemeer: PolicyInitiatedCancel 0, PolicyInitiatedPayPremium 1") {
        assertEncoding[PolicyInitiatedRedeemer](PolicyInitiatedCancel, constr(0))
        assertEncoding[PolicyInitiatedRedeemer](PolicyInitiatedPayPremium, constr(1))
    }

    test(
      "PolicyFundingRedeemer: PolicyFundingCancel 0, PolicyFundingFundingComplete 1 (POSIXTime)"
    ) {
        assertEncoding[PolicyFundingRedeemer](PolicyFundingCancel, constr(0))
        assertEncoding[PolicyFundingRedeemer](
          PolicyFundingFundingComplete(BigInt(1_709_295_132_000L)),
          constr(1, int(1_709_295_132_000L))
        )
    }

    test(
      "PolicyOnRiskRedeemer: PolicyOnRiskCreateClaim 0 (ClaimInfo) ... PolicyOnRiskClaimPayment 7"
    ) {
        assertEncoding[PolicyOnRiskRedeemer](PolicyOnRiskCreateClaim(claim), constr(0, claimData))
        assertEncoding[PolicyOnRiskRedeemer](PolicyOnRiskCloseClaim, constr(1))
        assertEncoding[PolicyOnRiskRedeemer](PolicyOnRiskAcceptClaim, constr(2))
        assertEncoding[PolicyOnRiskRedeemer](PolicyOnRiskFinalizeClaim, constr(3))
        assertEncoding[PolicyOnRiskRedeemer](PolicyOnRiskExpireClaim, constr(4))
        assertEncoding[PolicyOnRiskRedeemer](PolicyOnRiskFailClaim, constr(5))
        assertEncoding[PolicyOnRiskRedeemer](PolicyOnRiskCancel, constr(6))
        assertEncoding[PolicyOnRiskRedeemer](PolicyOnRiskClaimPayment, constr(7))
    }

    test(
      "InsurancePolicyRedeemer: PolicyInitiated 0, PolicyFunding 1, PolicyOnRisk 2, PolicyExpire 3"
    ) {
        assertEncoding[InsurancePolicyRedeemer](
          PolicyInitiated(PolicyInitiatedCancel),
          constr(0, constr(0))
        )
        assertEncoding[InsurancePolicyRedeemer](
          PolicyInitiated(PolicyInitiatedPayPremium),
          constr(0, constr(1))
        )
        assertEncoding[InsurancePolicyRedeemer](
          PolicyFunding(PolicyFundingCancel),
          constr(1, constr(0))
        )
        assertEncoding[InsurancePolicyRedeemer](
          PolicyFunding(PolicyFundingFundingComplete(42)),
          constr(1, constr(1, int(42)))
        )
        assertEncoding[InsurancePolicyRedeemer](
          PolicyOnRisk(PolicyOnRiskCreateClaim(claim)),
          constr(2, constr(0, claimData))
        )
        assertEncoding[InsurancePolicyRedeemer](
          PolicyOnRisk(PolicyOnRiskClaimPayment),
          constr(2, constr(7))
        )
        assertEncoding[InsurancePolicyRedeemer](PolicyExpire, constr(3))
    }

    test(
      "PiggyBankRedeemer follows its makeIsDataIndexed indices, not its Haskell declaration order"
    ) {
        // Haskell declares ClaimPremiumOnCancel before PayForClaimWithCollateral, but indexes
        // PayForClaimWithCollateral 3 and ClaimPremiumOnCancel 4
        assertEncoding[PiggyBankRedeemer](PayForClaimWithCollateral, constr(3))
        assertEncoding[PiggyBankRedeemer](ClaimPremiumOnCancel, constr(4))
        // the whole makeIsDataIndexed table
        val haskellIndices = Seq(
          BuyFidaCard -> 0,
          SellFidaCard -> 1,
          ClaimPremium -> 2,
          PayForClaimWithCollateral -> 3,
          ClaimPremiumOnCancel -> 4,
          UnlockCollateralOnCancel -> 5,
          UnlockCollateral -> 6
        )
        assert(haskellIndices.map(_._1).toSet == PiggyBankRedeemer.values.toSet)
        for (redeemer, index) <- haskellIndices do
            assertEncoding[PiggyBankRedeemer](redeemer, constr(index))
    }

    test("InvestorRedeemer: ProcessClaim 0, Withdraw 1") {
        assertEncoding(InvestorRedeemer.ProcessClaim, constr(0))
        assertEncoding(InvestorRedeemer.Withdraw, constr(1))
    }

    // Round trips -----------------------------------------------------------------------------

    private val genInteger: Gen[BigInt] = Gen.oneOf(
      Gen.choose(-1000L, 1000L).map(BigInt(_)),
      Arbitrary.arbitrary[Long].map(BigInt(_)),
      Arbitrary.arbitrary[BigInt]
    )
    private val genBytes: Gen[ByteString] =
        Gen.choose(0, 40)
            .flatMap(Gen.listOfN(_, Arbitrary.arbitrary[Byte]))
            .map(bs => ByteString.unsafeFromArray(bs.toArray))
    private val genHash: Gen[ByteString] =
        Gen.listOfN(28, Arbitrary.arbitrary[Byte]).map(bs => ByteString.unsafeFromArray(bs.toArray))
    private val genPkh: Gen[PubKeyHash] = genHash.map(PubKeyHash(_))
    private def genList[A](gen: Gen[A]): Gen[PList[A]] =
        Gen.choose(0, 5).flatMap(Gen.listOfN(_, gen)).map(PList.from)
    private def genOption[A](gen: Gen[A]): Gen[POption[A]] =
        Gen.option(gen).map(_.fold(POption.None)(POption.Some(_)))

    private val genCredential: Gen[Credential] = Gen.oneOf(
      genPkh.map(Credential.PubKeyCredential(_)),
      genHash.map(Credential.ScriptCredential(_))
    )
    private val genAddress: Gen[Address] = for
        credential <- genCredential
        staking <- genOption(
          Gen.oneOf(
            genCredential.map(StakingCredential.StakingHash(_)),
            Gen.zip(genInteger, genInteger, genInteger).map(StakingCredential.StakingPtr(_, _, _))
          )
        )
    yield Address(credential, staking)

    private val genState: Gen[InsurancePolicyState] = Gen.oneOf(InsurancePolicyState.values.toSeq)
    private val genAuthority: Gen[InsuranceAuthority] = Gen.oneOf(
      genPkh.map(SingleSign(_)),
      genList(genPkh).map(AtLeastOneSign(_)),
      genList(genPkh).map(AllMustSign(_)),
      genList(genPkh).map(MajorityMustSign(_))
    )
    private val genInstallments: Gen[InstallmentsInfo] =
        Gen.zip(genList(genInteger), genInteger).map(InstallmentsInfo(_, _))
    private val genClaim: Gen[ClaimInfo] = for
        amount <- genInteger
        date <- genInteger
        reason <- genBytes
        accepted <- Gen.oneOf(true, false)
        id <- genBytes
    yield ClaimInfo(amount, date, reason, accepted, id)

    private val genInsuranceInfo: Gen[InsurancePolicyDatum] = for
        ints <- Gen.listOfN(10, genInteger)
        installments <- genInstallments
        holder <- genPkh
        authority <- genAuthority
        state <- genState
        start <- genOption(genInteger)
        claim <- genOption(genClaim)
    yield InsuranceInfo(
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
    )
    private val genPolicyDatum: Gen[InsurancePolicyDatum] = Gen.oneOf(
      genInsuranceInfo,
      Gen.zip(genInteger, genList(genAddress)).map(PremiumPaymentInfo(_, _)),
      Gen.const(PolicyClaimPayment)
    )
    private val genPiggyBankDatum: Gen[PiggyBankDatum] = Gen.oneOf(
      Gen.zip(genInteger, genInteger).map(PBankPremium(_, _)),
      for
          sold <- Gen.oneOf(true, false)
          id <- genBytes
          value <- genInteger
          paid <- genList(genBytes)
      yield PBankFidaCard(sold, FidaCardId(id), value, paid)
    )
    private val genPolicyRedeemer: Gen[InsurancePolicyRedeemer] = Gen.oneOf(
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

    private def roundTrips[A: ToData: FromData](value: A): Unit =
        assert(fromData[A](value.toData) == value)

    test("fromData(toData(x)) == x for every datum type") {
        forAll(genPolicyDatum)(roundTrips(_))
        forAll(genPiggyBankDatum)(roundTrips(_))
        forAll(genAuthority)(roundTrips(_))
        forAll(genClaim)(roundTrips(_))
        forAll(genInstallments)(roundTrips(_))
    }

    test("fromData(toData(x)) == x for every redeemer type") {
        forAll(genPolicyRedeemer)(roundTrips(_))
        PiggyBankRedeemer.values.foreach(roundTrips(_))
        InvestorRedeemer.values.foreach(roundTrips(_))
    }

    test("fromData(toData(x)) == x for every script parameter type") {
        forAll(genHash) { hash =>
            roundTrips(InsuranceId(hash))
            roundTrips(SystemId(hash))
            roundTrips(PubKeyHash(hash))
        }
        forAll(genBytes)(id => roundTrips(FidaCardId(id)))
        forAll(Gen.listOfN(32, Arbitrary.arbitrary[Byte]), Gen.choose(0, 1000)) { (txId, index) =>
            roundTrips(TxOutRef(TxId(ByteString.unsafeFromArray(txId.toArray)), index))
        }
        forAll(genInteger)(roundTrips(_))
    }
}
