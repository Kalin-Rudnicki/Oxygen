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

final case class Note(
    @primaryKey id: UUID,
    @references[Person] personId: UUID,
    note: String,
)
object Note extends TableCompanion[Note, UUID](TableRepr.derived[Note]) {

  private val randomNote: UIO[String] = RandomGen.lowerCaseString(5).replicateZIO(5).map(_.mkString(" "))

  def generate(personId: UUID)(
      id: Specified[UUID] = Specified.WasNotSpecified,
      note: Specified[String] = Specified.WasNotSpecified,
  ): UIO[Note] =
    for {
      id <- id.orGen { Random.nextUUID }
      note <- note.orGen { randomNote }
    } yield Note(id, personId, note)

}

final case class MultiPK1(
    @primaryKey id1: String,
    @primaryKey id2: String,
    value: Int,
)
object MultiPK1 extends TableCompanion[MultiPK1, (String, String)](TableRepr.derived[MultiPK1])

@foreignKey[MultiPK2, MultiPK1]((_.id1Ref, _.id1), (_.id2Ref, _.id2))
final case class MultiPK2(
    id1Ref: String,
    id2Ref: String,
    value: Int,
)
object MultiPK2 extends TableCompanion[MultiPK2, Unit](TableRepr.derived[MultiPK2])

final case class Ints(
    a: Int,
    b: Int,
)
object Ints extends TableCompanion[Ints, Unit](TableRepr.derived[Ints])

final case class Arrays(
    _1: List[Int],
    _2: Set[String],
    _3: List[List[Boolean]],
    _4: Option[ArraySeq[String]],
)
object Arrays extends TableCompanion[Arrays, Unit](TableRepr.derived[Arrays])

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

  @compile
  val personJoinNotes: QueryIO[UUID, (Person, Note)] =
    for {
      i <- input[UUID]
      p1 <- select[Person]
      n1 <- join[Note] if n1.personId == p1.id
      _ <- where if p1.groupId == i
    } yield (p1, n1)

  @compile
  val personLeftJoinNotes: QueryIO[UUID, (Person, Option[Note])] =
    for {
      i <- input[UUID]
      p1 <- select[Person]
      n1 <- leftJoin[Note] if n1.personId == p1.id
      _ <- where if p1.groupId == i
    } yield (p1, n1)

  @compile
  val intsConstLimit: QueryO[Ints] =
    for {
      i <- select[Ints]
      _ <- limit(const(5))
    } yield i

  @compile
  val intsDynamicLimit: QueryIO[Int, Ints] =
    for {
      l <- input[Int]
      i <- select[Ints]
      _ <- limit(l)
    } yield i

  @compile
  val intsOrderByA: QueryIO[Int, Ints] =
    for {
      l <- input[Int]
      i <- select[Ints]
      _ <- orderBy(i.a.asc)
      _ <- limit(l)
    } yield i

  @compile
  val intsOrderByAB: QueryIO[Int, Ints] =
    for {
      l <- input[Int]
      i <- select[Ints]
      _ <- orderBy(i.a.asc, i.b.desc)
      _ <- limit(l)
    } yield i

  @compile
  val intsOrderByBA: QueryIO[Int, Ints] =
    for {
      l <- input[Int]
      i <- select[Ints]
      _ <- orderBy(i.b.desc, i.a.asc)
      _ <- limit(l)
    } yield i

  @compile
  val intsOrderByABOffset: QueryIO[(Int, Int), Ints] =
    for {
      l <- input[Int]
      o <- input[Int]
      i <- select[Ints]
      _ <- orderBy(i.a.asc, i.b.desc)
      _ <- limit(l)
      _ <- offset(o)
    } yield i

  @compile
  val personSearch: QueryIO[(Option[String], Option[String]), Person] =
    for {
      first <- input.optional[String]
      last <- input.optional[String]
      p <- select[Person]
      _ <- where if p.first == first && p.last == last
    } yield p

  @compile
  val personSearchCountConst: QueryIO[(Option[String], Option[String]), Long] =
    for {
      first <- input.optional[String]
      last <- input.optional[String]
      p <- select[Person]
      _ <- where if p.first == first && p.last == last
    } yield count.*

  @compile
  val personSearchCountNonConst: QueryIO[(Option[String], Option[String]), Long] =
    for {
      first <- input.optional[String]
      last <- input.optional[String]
      p <- select[Person]
      _ <- where if p.first == first && p.last == last
    } yield count(p)

  //////////////////////////////////////////////////////////////////////////////////////////////////////
  //      Update
  //////////////////////////////////////////////////////////////////////////////////////////////////////

  @compile
  val setAgeTo0_oldSyntax: QueryIO[Person, Person] =
    for {
      zero <- input.const(0)
      i <- input[Person]
      (p, set) <- update[Person]
      _ <- where if p.tablePK == i.tablePK
      _ <- set(_.age := zero)
    } yield p

  @compile
  val setAgeTo0_newSyntax: QueryIO[Person, Person] =
    for {
      i <- input[Person]
      (p, set) <- update[Person]
      _ <- where if p.tablePK == i.tablePK
      _ <- set(_.age := const(0))
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
