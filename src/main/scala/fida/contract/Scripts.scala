package fida.contract

import fida.contract.insurance.*
import scalus.cardano.ledger.{Language, MajorProtocolVersion, Script}
import scalus.compiler.Options
import scalus.compiler.sir.TargetLoweringBackend
import scalus.uplc.{CompiledPlutus, PlutusV2, Program}
import scalus.uplc.builtin.Data
import scalus.uplc.builtin.Data.toData
import scalus.cardano.onchain.plutus.v1.{PubKeyHash, TxOutRef}

/** Compiled Fida contracts.
  *
  * Each contract is compiled unapplied, taking its parameters as `Data` (the equivalent of the
  * Haskell `serialisable*` scripts), so that off-chain code can apply parameters itself, exactly
  * like cardano-transaction-lib's `applyArgs`. The `*Script` helpers apply parameters the same way:
  * by wrapping the program in UPLC applications to `Data` constants, without recompiling.
  */
object Scripts {

    /** Plutus V2 (as the Haskell contracts). Scripts target protocol version 10 (Plomin) so they
      * are plain UPLC 1.0.0 programs that existing off-chain tooling can handle; they run unchanged
      * on later protocol versions. Error-code traces (`ERR-…`) are kept, as in the Haskell build.
      */
    given options: Options = Options(
      targetLoweringBackend = TargetLoweringBackend.SirToUplcV3Lowering,
      targetLanguage = Language.PlutusV2,
      targetProtocolVersion = MajorProtocolVersion.plominPV,
      generateErrorTraces = true,
      removeTraces = false,
      optimizeUplc = true,
      // silences lowering warnings about prelude types whose UPLC 1.1.0 representation is
      // unavailable at this protocol version (they fall back to the Data representation)
      noWarn = true
    )

    /** Parameter: `TxOutRef`. */
    lazy val insuranceIdMintingPolicy: PlutusV2[Data => Data => Data => Unit] =
        PlutusV2.compile((ref: Data) =>
            (redeemer: Data) => (ctx: Data) => InsuranceIdMintingPolicy.untyped(ref, redeemer, ctx)
        )

    /** Parameter: [[InsuranceId]]. */
    lazy val insurancePolicyValidator: PlutusV2[Data => Data => Data => Data => Unit] =
        PlutusV2.compile((iid: Data) =>
            (datum: Data) =>
                (redeemer: Data) =>
                    (ctx: Data) => InsurancePolicyValidator.untyped(iid, datum, redeemer, ctx)
        )

    /** Parameters: [[InsuranceId]], [[FidaCardId]]. */
    lazy val piggyBankValidator: PlutusV2[Data => Data => Data => Data => Data => Unit] =
        PlutusV2.compile((iid: Data) =>
            (fcid: Data) =>
                (datum: Data) =>
                    (redeemer: Data) =>
                        (ctx: Data) => PiggyBankValidator.untyped(iid, fcid, datum, redeemer, ctx)
        )

    /** Parameters: investor `PubKeyHash`, [[SystemId]]. */
    lazy val investorValidator: PlutusV2[Data => Data => Data => Data => Data => Unit] =
        PlutusV2.compile((investor: Data) =>
            (systemId: Data) =>
                (datum: Data) =>
                    (redeemer: Data) =>
                        (ctx: Data) =>
                            InvestorValidator.untyped(investor, systemId, datum, redeemer, ctx)
        )

    /** Parameters: system governance `PubKeyHash`, system magic `Integer`. */
    lazy val systemIdMintingPolicy: PlutusV2[Data => Data => Data => Data => Unit] =
        PlutusV2.compile((governance: Data) =>
            (magic: Data) =>
                (redeemer: Data) =>
                    (ctx: Data) => SystemIdMintingPolicy.untyped(governance, magic, redeemer, ctx)
        )

    /** The unapplied scripts under the names used by the Haskell `fida-contracts-serialise` tool.
      */
    lazy val serialisable: Seq[(String, CompiledPlutus[?])] = Seq(
      "InsuranceIdMintingPolicy" -> insuranceIdMintingPolicy,
      "InsurancePolicyValidator" -> insurancePolicyValidator,
      "InvestorValidator" -> investorValidator,
      "SystemIdMintingPolicy" -> systemIdMintingPolicy,
      "PiggyBankValidator" -> piggyBankValidator
    )

    /** Applies `params` to `compiled` at the UPLC level, like `applyArgs`. */
    def applyParams(compiled: CompiledPlutus[?], params: Data*): Program =
        params.foldLeft(compiled.program)(_ $ _)

    def insuranceIdMintingPolicyScript(ref: TxOutRef): Script.PlutusV2 =
        Script.PlutusV2(applyParams(insuranceIdMintingPolicy, ref.toData))

    def insurancePolicyScript(iid: InsuranceId): Script.PlutusV2 =
        Script.PlutusV2(applyParams(insurancePolicyValidator, iid.toData))

    def piggyBankScript(iid: InsuranceId, fcid: FidaCardId): Script.PlutusV2 =
        Script.PlutusV2(applyParams(piggyBankValidator, iid.toData, fcid.toData))

    def investorScript(investor: PubKeyHash, systemId: SystemId): Script.PlutusV2 =
        Script.PlutusV2(applyParams(investorValidator, investor.toData, systemId.toData))

    def systemIdMintingPolicyScript(governance: PubKeyHash, magic: BigInt): Script.PlutusV2 =
        Script.PlutusV2(applyParams(systemIdMintingPolicy, governance.toData, magic.toData))
}
