package oxygen.sql.query

import oxygen.predef.core.*
import oxygen.sql.query.dsl.Q
import oxygen.sql.query.dsl.Q.*
import oxygen.sql.schema.*

abstract class TableCompanion[A, K](derivedRepr: TableRepr.AuxPK[A, K]) {

  final given tableRepr: TableRepr.AuxPK[A, K] = derivedRepr

  final val insert: QueryI[A] =
    QueryI.compile("insert") {
      for {
        i <- input[A]
        (_, into) <- Q.insert[A](using tableRepr)
        _ <- into(i)
      } yield ()
    }

  final lazy val batchOptimizedInsert: BatchOptimizedInsert[A] = BatchOptimizedInsert.unsafeParse(insert)

  final lazy val upsert: QueryI[A] = {
    val pkCols = tableRepr.pk.rowRepr.columns.columns
    val npkCols = tableRepr.npk.rowRepr.columns.columns

    if (pkCols.isEmpty) throw new RuntimeException("Can not generate upsert query for table with no primary-key columns")

    val onConflictClause: String = s"ON CONFLICT (${pkCols.map(_.name).mkString(", ")})"

    val onConflict: String =
      if (npkCols.nonEmpty)
        s"""
           |    $onConflictClause
           |    DO UPDATE
           |    SET ${npkCols.map { c => s"${c.name} = EXCLUDED.${c.name}" }.mkString(",\n        ")}""".stripMargin
      else
        s"""
           |    $onConflictClause
           |    DO NOTHING""".stripMargin

    QueryI(
      insert.ctx.copy(
        queryName = "upsertByPK",
        queryType = QueryContext.QueryType.Upsert,
        sql = insert.ctx.sql + onConflict,
      ),
      insert.encoder,
    )
  }

  final lazy val batchOptimizedUpsert: BatchOptimizedInsert[A] = BatchOptimizedInsert.unsafeParse(upsert)

  final val selectAll: QueryO[A] =
    QueryO.compile("selectAll") {
      for {
        a <- select[A](using tableRepr)
      } yield a
    }

  final val selectByPK: QueryIO[K, A] =
    QueryIO.compile("selectByPK") {
      for {
        i <- input[K]
        a <- select[A](using tableRepr)
        _ <- where if a.tablePK == i
      } yield a
    }

  final val update: QueryI[A] =
    QueryI.compile("updateByPK") {
      for {
        i <- input[A]
        (a, set) <- Q.update[A](using tableRepr)
        _ <- where if a.tablePK == i.tablePK
        _ <- set(_.tableNPK := i.tableNPK)
      } yield ()
    }

  final val deleteByPK: QueryI[K] =
    QueryI.compile("updateByPK") {
      for {
        i <- input[K]
        a <- delete[A](using tableRepr)
        _ <- where if a.tablePK == i
      } yield ()
    }

  final val truncate: Query =
    Query.simple("truncate", QueryContext.QueryType.Truncate)(s"TRUNCATE ${tableRepr.ref}")

  final val truncateCascade: Query =
    Query.simple("truncate-cascade", QueryContext.QueryType.Truncate)(s"TRUNCATE ${tableRepr.ref} CASCADE")

  final val select_* : QueryO[Long] =
    QueryO(QueryContext("COUNT(*)", s"SELECT COUNT(*) FROM ${tableRepr.ref}", QueryContext.QueryType.Select, tableRepr.some), None, RowRepr.long.decoder)

}
object TableCompanion {

  abstract class NoKey[A](derivedRepr: TableRepr.AuxPK[A, Unit]) {

    final given tableRepr: TableRepr.AuxPK[A, Unit] = derivedRepr

    final val selectAll: QueryO[A] =
      QueryO.compile("selectAll") {
        for {
          a <- select[A](using tableRepr)
        } yield a
      }

  }

}
