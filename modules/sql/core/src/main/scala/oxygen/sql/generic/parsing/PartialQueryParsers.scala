package oxygen.sql.generic.parsing

import oxygen.quoted.*
import oxygen.sql.generic.model.part.*

trait PartialQueryParsers[A] {

  lazy val partialParser: MapChainParser[A]

  final lazy val fullParserEmptyRefs: Parser[Term, FullQueryResult[A]] =
    (InputPart.many.withContext("Input") >>> partialParser).withReturningEmptyRefs.map(FullQueryResult(_, _, _, _))

  final lazy val fullParserAcceptingRefs: Parser[(Term, RefMap), FullQueryResult[A]] =
    (InputPart.many.withContext("Input") >>> partialParser).withReturningAcceptingRefs.map(FullQueryResult(_, _, _, _))

}
