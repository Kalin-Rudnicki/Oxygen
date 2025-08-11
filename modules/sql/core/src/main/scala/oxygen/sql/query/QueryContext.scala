package oxygen.sql.query

import oxygen.predef.core.*
import oxygen.sql.SqlMetrics
import oxygen.sql.query.QueryContext.ExecutionType
import oxygen.sql.schema.TableRepr
import oxygen.zio.*
import zio.*
import zio.metrics.*

final case class QueryContext(
    queryName: String,
    sql: String,
    queryType: QueryContext.QueryType,
    mainTable: Option[TableRepr[?]] = None,
    constParams: Seq[(String, String)] = Nil,
) {

  val queryNamePrefix: String = mainTable match
    case Some(mainTable) => s"${mainTable.ref} - "
    case None            => ""

  private val constParamsStr: String =
    if (constParams.isEmpty) ""
    else constParams.map { case (k, v) => s"$k: $v" }.mkString(" (", ", ", ")")
  val queryContextHeader: String = s"[$queryType] $queryNamePrefix$queryName$constParamsStr"

  object metrics {

    private val defaultMetricLabels: Set[MetricLabel] =
      Seq[IterableOnce[MetricLabel]](
        MetricLabel("query.name", queryName).some,
        MetricLabel("query.type", queryType.toString).some,
        mainTable.map { t => MetricLabel("query.main-table", t.ref) },
      ).flatten.toSet

    private val queryDuration: Metric.Histogram[Duration] = SqlMetrics.queryDuration.tagged(defaultMetricLabels)

    def track(exeType: QueryContext.ExecutionType): ZIOAspectPoly =
      new ZIOAspectPoly.Impl {
        override def apply[R, E, A](effect: ZIO[R, E, A])(implicit trace: Trace): ZIO[R, E, A] =
          effect.exit.timed.flatMap { case (duration, exit) =>
            val successLabel =
              MetricLabel(
                "query.result-kind",
                exit match {
                  case Exit.Success(_)     => "success"
                  case Exit.Failure(cause) =>
                    if (cause.isFailure) "failure"
                    else if (cause.isDie) "defect"
                    else if (cause.isInterrupted) "interrupt"
                    else "unknown"
                },
              )

            val extraLabels: Set[MetricLabel] =
              exeType match {
                case ExecutionType.Query =>
                  Set(MetricLabel("query.execution-type", "query"), successLabel)
                case ExecutionType.Update =>
                  Set(MetricLabel("query.execution-type", "update"), successLabel)
                case ExecutionType.JdbcBatchUpdate(batchSize) =>
                  Set(MetricLabel("query.execution-type", "jdbc-batch-update"), MetricLabel("query.batch-size", batchSize.toString), successLabel)
                case ExecutionType.AggregatedBatchUpdate(batchSize) =>
                  Set(MetricLabel("query.execution-type", "aggregated-batch-update"), MetricLabel("query.batch-size", batchSize.toString), successLabel)
              }

            // TODO (KR) : add tracking for {result, update, batch-update} +efficient-batch-update?
            queryDuration.tagged(extraLabels).update(duration) *>
              exit
          }
      }

  }

}
object QueryContext {

  enum QueryType { case Select, Insert, Update, Delete, Transaction, Truncate, Migrate }

  enum ExecutionType {
    case Query
    case Update
    case JdbcBatchUpdate(batchSize: Int)
    case AggregatedBatchUpdate(batchSize: Int)
  }

}
