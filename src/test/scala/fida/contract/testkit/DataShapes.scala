package fida.contract.testkit

import org.scalacheck.Gen
import scalus.cardano.onchain.plutus.prelude.List as PList
import scalus.uplc.builtin.{ByteString, Data}

/** The shape of a type's `Data` encoding, as PlutusTx `makeIsDataIndexed` and the plutus-tx and
  * plutus-ledger-api instances define it.
  */
enum Shape:
    case Integer
    case Bytes
    case ListOf(element: Shape)

    /** Constructors by tag, with the shapes of their fields. */
    case Constrs(alternatives: Map[BigInt, Seq[Shape]])

/** The shapes of the contracts' datums and redeemers, the check PlutusTx `fromBuiltinData` makes,
  * and generators of conforming and malformed data.
  */
object Shape {

    def constrs(alternatives: (Int, Seq[Shape])*): Shape =
        Constrs(alternatives.map((tag, fields) => BigInt(tag) -> fields).toMap)

    // plutus-tx and plutus-ledger-api (V2) types

    val bool: Shape = constrs(0 -> Seq(), 1 -> Seq())
    def maybe(a: Shape): Shape = constrs(0 -> Seq(a), 1 -> Seq())
    val credential: Shape = constrs(0 -> Seq(Bytes), 1 -> Seq(Bytes))
    val stakingCredential: Shape =
        constrs(0 -> Seq(credential), 1 -> Seq(Integer, Integer, Integer))
    val address: Shape = constrs(0 -> Seq(credential, maybe(stakingCredential)))

    // Fida types

    val installmentsInfo: Shape = constrs(0 -> Seq(ListOf(Integer), Integer))
    val authority: Shape = constrs(
      0 -> Seq(Bytes),
      1 -> Seq(ListOf(Bytes)),
      2 -> Seq(ListOf(Bytes)),
      3 -> Seq(ListOf(Bytes))
    )
    val policyState: Shape = constrs((0 to 4).map(_ -> Seq())*)
    val claimInfo: Shape = constrs(0 -> Seq(Integer, Integer, Bytes, bool, Bytes))
    private val insuranceInfoFields: Seq[Shape] = Seq(
      Integer, // iInfoCollateralAmount
      Integer, // iInfoFidaCardValue
      Integer, // iInfoFidaCardNumber
      Integer, // iInfoPremiumAmount
      installmentsInfo,
      Integer, // iInfoInsurancePeriod
      Bytes, // iInfoPolicyHolder
      authority,
      policyState,
      Integer, // iInfoFundingDeadline
      Integer, // iInfoTotalClaimsAcceptedAmount
      Integer, // iInfoClaimTimeToLive
      Integer, // iInfoClaimTimeToPay
      maybe(Integer), // iInfoStartDate
      maybe(claimInfo) // iInfoClaim
    )
    private val premiumPaymentInfoFields: Seq[Shape] = Seq(Integer, ListOf(address))
    val insuranceInfo: Shape = constrs(0 -> insuranceInfoFields)
    val premiumPaymentInfo: Shape = constrs(1 -> premiumPaymentInfoFields)
    val insurancePolicyDatum: Shape =
        constrs(0 -> insuranceInfoFields, 1 -> premiumPaymentInfoFields, 2 -> Seq())
    val pBankPremium: Shape = constrs(0 -> Seq(Integer, Integer))
    val piggyBankDatum: Shape =
        constrs(0 -> Seq(Integer, Integer), 1 -> Seq(bool, Bytes, Integer, ListOf(Bytes)))
    val insurancePolicyRedeemer: Shape = constrs(
      0 -> Seq(constrs(0 -> Seq(), 1 -> Seq())),
      1 -> Seq(constrs(0 -> Seq(), 1 -> Seq(Integer))),
      2 -> Seq(constrs((0 -> Seq(claimInfo)) +: (1 to 7).map(_ -> Seq())*)),
      3 -> Seq()
    )
    val piggyBankRedeemer: Shape = constrs((0 to 6).map(_ -> Seq())*)

    /** Whether PlutusTx `fromBuiltinData` decodes `d` as `shape`: a known constructor with exactly
      * its fields, a list whose elements all decode, an integer or a bytestring.
      */
    def conforms(shape: Shape, d: Data): Boolean = (shape, d) match
        case (Integer, Data.I(_))                 => true
        case (Bytes, Data.B(_))                   => true
        case (ListOf(element), Data.List(values)) => values.toScalaList.forall(conforms(element, _))
        case (Constrs(alternatives), Data.Constr(tag, args)) =>
            alternatives.get(tag).exists { fields =>
                val values = args.toScalaList
                values.size == fields.size && fields.lazyZip(values).forall(conforms)
            }
        case _ => false

    def constrData(tag: BigInt, fields: Seq[Data]): Data = Data.Constr(tag, PList.from(fields))

    def listData(values: Seq[Data]): Data = Data.List(PList.from(values))

    /** The bytestring [[covering]] uses for every `Bytes` value. */
    val sampleBytes: ByteString = ByteString.fromHex("ab" * 28)

    /** A few values of `shape` that together use every constructor of every type it contains, and
      * both empty and non-empty lists.
      */
    def covering(shape: Shape): Seq[Data] = shape match
        case Integer               => Seq(Data.I(42))
        case Bytes                 => Seq(Data.B(sampleBytes))
        case ListOf(element)       => Seq(listData(Seq()), listData(covering(element)))
        case Constrs(alternatives) =>
            alternatives.toSeq.sortBy(_._1).flatMap { (tag, fields) =>
                val columns = fields.map(covering)
                val rows = columns.map(_.size).maxOption.getOrElse(1)
                (0 until rows).map(i => constrData(tag, columns.map(c => c(i % c.size))))
            }

    /** Data conforming to `shape`, with short lists. */
    def gen(shape: Shape): Gen[Data] = shape match
        case Integer         => Generators.genInteger.map(Data.I(_))
        case Bytes           => Generators.genBytes.map(Data.B(_))
        case ListOf(element) => Gen.choose(0, 3).flatMap(Gen.listOfN(_, gen(element))).map(listData)
        case Constrs(alternatives) =>
            Gen.oneOf(alternatives.toSeq).flatMap { (tag, fields) =>
                fields
                    .foldRight(Gen.const(List.empty[Data]))((field, rest) =>
                        gen(field).flatMap(d => rest.map(d :: _))
                    )
                    .map(constrData(tag, _))
            }

    /** A change that makes data of some shape malformed. */
    enum Corruption:
        /** A value of another kind, e.g. a bytestring where an integer is expected. */
        case WrongKind

        /** A constructor tag the type does not have. */
        case UnknownTag

        /** One field more than the constructor has. */
        case ExtraField

        /** One field less than the constructor has. */
        case MissingField

    /** The nodes of `d`, which conforms to `shape`: each node's path (indices of constructor fields
      * and list elements), shape and value, the root first.
      */
    def nodes(shape: Shape, d: Data): Seq[(Vector[Int], Shape, Data)] =
        def go(shape: Shape, d: Data, path: Vector[Int]): Seq[(Vector[Int], Shape, Data)] =
            val children = (shape, d) match
                case (ListOf(element), Data.List(values)) =>
                    values.toScalaList.zipWithIndex.flatMap((v, i) => go(element, v, path :+ i))
                case (Constrs(alternatives), Data.Constr(tag, args)) =>
                    args.toScalaList.zip(alternatives(tag)).zipWithIndex.flatMap {
                        case ((v, s), i) => go(s, v, path :+ i)
                    }
                case _ => Seq.empty
            (path, shape, d) +: children
        go(shape, d, Vector.empty)

    /** `d` with the node at `path` replaced by `f` of it. */
    def updated(d: Data, path: Seq[Int], f: Data => Data): Data =
        path match
            case Seq()     => f(d)
            case i +: rest =>
                d match
                    case Data.List(values) =>
                        val xs = values.toScalaList
                        listData(xs.updated(i, updated(xs(i), rest, f)))
                    case Data.Constr(tag, args) =>
                        val xs = args.toScalaList
                        constrData(tag, xs.updated(i, updated(xs(i), rest, f)))
                    case other => throw IllegalArgumentException(s"no child $i in $other")

    /** One value of each kind of `Data`, but `shape`'s own kind. */
    def otherKinds(shape: Shape): Seq[Data] =
        val integer = Data.I(7)
        val bytes = Data.B(ByteString.fromString("x"))
        val list = listData(Seq(Data.I(1)))
        val map = Data.Map(PList((Data.I(1), Data.I(2))))
        val constr = constrData(0, Seq())
        shape match
            case Integer    => Seq(bytes, list, map, constr)
            case Bytes      => Seq(integer, list, map, constr)
            case ListOf(_)  => Seq(integer, bytes, map, constr)
            case Constrs(_) => Seq(integer, bytes, list, map)

    /** Every way to apply `corruption` once to `d`, which conforms to `shape`. */
    def corruptions(shape: Shape, d: Data, corruption: Corruption): Seq[Data] =
        nodes(shape, d).flatMap { (path, nodeShape, node) =>
            corrupted(nodeShape, node, corruption).map(c => updated(d, path, _ => c))
        }

    /** `node`, of shape `nodeShape`, changed by `corruption`, in every way that applies. */
    private def corrupted(nodeShape: Shape, node: Data, corruption: Corruption): Seq[Data] =
        (corruption, nodeShape, node) match
            case (Corruption.WrongKind, _, _) => otherKinds(nodeShape)
            case (Corruption.UnknownTag, Constrs(alternatives), Data.Constr(_, args)) =>
                Seq(Data.Constr(alternatives.keys.max + 1, args))
            case (Corruption.ExtraField, Constrs(_), Data.Constr(tag, args)) =>
                Seq(constrData(tag, args.toScalaList :+ Data.I(0)))
            case (Corruption.MissingField, Constrs(_), Data.Constr(tag, args)) if !args.isEmpty =>
                Seq(constrData(tag, args.toScalaList.init))
            case _ => Seq.empty

    /** `d`, which conforms to `shape`, with `corruption` applied at a random node where it applies,
      * or `d` itself when it applies nowhere.
      */
    def genCorrupted(shape: Shape, d: Data, corruption: Corruption): Gen[Data] =
        corruptions(shape, d, corruption) match
            case Seq() => Gen.const(d)
            case cases => Gen.oneOf(cases)
}
