package oxygen.sql.generic.parsing

import oxygen.quoted.*

final case class MapChainResult[+A](
    value: A,
    mapFunctName: String,
    newRefs: RefMap,
    mapFunctBody: Term,
)

type MapChainParseResult[+A] = ParseResult[MapChainResult[A]]
