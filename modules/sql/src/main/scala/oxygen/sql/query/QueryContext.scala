package oxygen.sql.query

final case class QueryContext(
    queryName: String,
    sql: String,
    queryType: QueryContext.QueryType,
    // TODO (KR) : include info about which tables are accessed
)
object QueryContext {

  enum QueryType { case Select, Insert, Update, Delete, Transaction, Truncate, Migrate }

}
