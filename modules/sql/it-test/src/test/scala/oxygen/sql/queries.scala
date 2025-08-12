package oxygen.sql

import java.util.UUID
import oxygen.predef.core.*
import oxygen.sql.query.*
import oxygen.sql.query.dsl.*
import oxygen.sql.query.dsl.Q.*
import oxygen.sql.schema.*
import oxygen.test.RandomGen
import oxygen.test.RandomGen.syntax.*
import scala.annotation.experimental
import zio.*

// @oxygen.meta.K0.annotation.showDerivation[TableRepr]
final case class Person(
    @primaryKey id: UUID,
    groupId: UUID,
    first: String,
    last: String,
    age: Int,
)
object Person extends TableCompanion[Person, UUID](TableRepr.derived[Person]) {

  private val randomName: UIO[String] = RandomGen.capitalizedString()

  def generate(groupId: UUID)(
      id: Specified[UUID] = Specified.WasNotSpecified,
      first: Specified[String] = Specified.WasNotSpecified,
      last: Specified[String] = Specified.WasNotSpecified,
      age: Specified[Int] = Specified.WasNotSpecified,
  ): UIO[Person] =
    for {
      id <- id.orGen { Random.nextUUID }
      first <- first.orGen { randomName }
      last <- last.orGen { randomName }
      age <- age.orGen { Random.nextIntBetween(0, 100) }
    } yield Person(id, groupId, first, last, age)

}

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
