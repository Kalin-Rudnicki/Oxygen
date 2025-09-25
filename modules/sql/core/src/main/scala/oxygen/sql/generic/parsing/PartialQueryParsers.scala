package oxygen.sql.generic.parsing

import oxygen.quoted.*
import oxygen.sql.generic.model.part.*

trait PartialQueryParsers[A] {

  lazy val partialParser: MapChainParser[A]

  final lazy val fullParser: Parser[Term, FullQueryResult[A]] =
    (InputPart.many.withContext("Input") >>> partialParser).withReturning.map(FullQueryResult(_, _, _, _))

}
