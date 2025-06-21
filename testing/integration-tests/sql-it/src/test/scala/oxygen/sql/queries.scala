package oxygen.sql

import java.util.UUID
import oxygen.core.collection.Contiguous
import oxygen.sql.query.*
import oxygen.sql.query.dsl.*
import oxygen.sql.query.dsl.Q.*
import oxygen.sql.schema.*
import scala.annotation.experimental

// @oxygen.meta.K0.annotation.showDerivation[TableRepr]
final case class Person(
    @primaryKey id: UUID,
    groupId: UUID,
    first: String,
    last: String,
    age: Int,
)
object Person extends TableCompanion[Person, UUID](TableRepr.derived[Person])

@experimental
object queries {

  //////////////////////////////////////////////////////////////////////////////////////////////////////
  //      Select
  //////////////////////////////////////////////////////////////////////////////////////////////////////

  @compile
  val selectByGroupId: QueryIO[UUID, Person] =
    for {
      i <- input[UUID]
      p1 <- select[Person]
      _ <- where if p1.groupId == i
    } yield p1

  @compile
  val othersWithSameLastNameAsId: QueryIO[UUID, Person] =
    for {
      i <- input[UUID]
      p1 <- select[Person]
      p2 <- join[Person] if p2.last == p1.last && p2.id != p1.id
      _ <- where if p1.id == i
    } yield p2

  //////////////////////////////////////////////////////////////////////////////////////////////////////
  //      Update
  //////////////////////////////////////////////////////////////////////////////////////////////////////

  @compile
  val setAgeTo0: QueryIO[Person, Person] =
    for {
      zero <- input.const(0)
      i <- input[Person]
      (p, set) <- update[Person]
      _ <- where if p.tablePK == i.tablePK
      _ <- set(_.age := zero)
    } yield p

  //////////////////////////////////////////////////////////////////////////////////////////////////////
  //      Select
  //////////////////////////////////////////////////////////////////////////////////////////////////////

  @compile
  val deleteByGroupId: QueryIO[UUID, Person] =
    for {
      i <- input[UUID]
      p1 <- delete[Person]
      _ <- where if p1.groupId == i
    } yield p1

}
