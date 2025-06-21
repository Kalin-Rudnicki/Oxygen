package oxygen.sql.query

import oxygen.sql.schema.TableRepr

final case class QueryContext(
    queryName: String,
    sql: String,
    queryType: QueryContext.QueryType,
    mainTable: Option[TableRepr[?]] = None,
) {

  val queryNamePrefix: String = mainTable match
    case Some(mainTable) => s"${mainTable.ref} - "
    case None            => ""

  val queryContextHeader: String = s"[$queryType] $queryNamePrefix$queryName"

}
object QueryContext {

  enum QueryType { case Select, Insert, Update, Delete, Transaction, Truncate, Migrate }

}
