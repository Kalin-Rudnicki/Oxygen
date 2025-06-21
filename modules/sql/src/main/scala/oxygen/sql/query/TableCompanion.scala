package oxygen.sql.query

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
