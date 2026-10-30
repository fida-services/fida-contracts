package scalus.cardano.onchain.plutus.prelude

/** `=!=`, the inequality stryker4s writes when it mutates `===` (it assumes the cats operator;
  * Scalus spells inequality `!==`). It exists only in the `jvm` build, where it lets those mutants
  * compile and run instead of being discarded as compile errors.
  */
extension [A](x: A) inline infix def =!=(inline y: A)(using inline eq: Eq[A]): Boolean = !eq(x, y)
