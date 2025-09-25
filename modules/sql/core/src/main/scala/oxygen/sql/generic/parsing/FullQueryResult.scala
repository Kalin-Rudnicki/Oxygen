package oxygen.sql.generic.parsing

import oxygen.sql.generic.model.part.*

final case class FullQueryResult[+A](
    inputs: List[InputPart],
    value: A,
    returning: ReturningPart,
    refMap: RefMap,
)
