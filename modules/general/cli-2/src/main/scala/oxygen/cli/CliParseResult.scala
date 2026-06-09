package oxygen.cli

sealed trait CliParseResult[+V, A] {

  final def mapRemaining[A2](f: A => A2): CliParseResult[V, A2] =
    ??? // FIX-PRE-MERGE (KR) :

}
object CliParseResult {

  final case class Success[+V, A](
      value: V,
      remaining: A,
  )

  // FIX-PRE-MERGE (KR) : errors

}
