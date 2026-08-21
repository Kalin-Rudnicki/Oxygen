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
      id: Specified[UUID] = ___,
      first: Specified[String] = ___,
      last: Specified[String] = ___,
      age: Specified[Int] = ___,
  ): UIO[Person] =
    for {
      id <- id.orGen { Random.nextUUID }
      first <- first.orGen { randomName }
      last <- last.orGen { randomName }
      age <- age.orGen { Random.nextIntBetween(0, 100) }
    } yield Person(id, groupId, first, last, age)

}

// @oxygen.meta.K0.annotation.showDerivation[TableRepr]
final case class PersonCache(
    @primaryKey id: UUID,
    first: String,
    last: String,
    age: Int,
) {

  def toPerson(groupId: UUID): Person =
    Person(
      id = id,
      groupId = groupId,
      first = first,
      last = last,
      age = age,
    )

}
object PersonCache extends TableCompanion[PersonCache, UUID](TableRepr.derived[PersonCache]) {

  private val randomName: UIO[String] = RandomGen.capitalizedString()

  def generate(
      id: Specified[UUID] = Specified.WasNotSpecified,
      first: Specified[String] = Specified.WasNotSpecified,
      last: Specified[String] = Specified.WasNotSpecified,
      age: Specified[Int] = Specified.WasNotSpecified,
  ): UIO[PersonCache] =
    for {
      id <- id.orGen { Random.nextUUID }
      first <- first.orGen { randomName }
      last <- last.orGen { randomName }
      age <- age.orGen { Random.nextIntBetween(0, 100) }
    } yield PersonCache(id, first, last, age)

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

final case class Note2(
    @primaryKey id: UUID,
    @references[Person] personId: UUID,
    note: String,
    note2: Option[String],
)
object Note2 extends TableCompanion[Note2, UUID](TableRepr.derived[Note2]) {

  private val randomNote: UIO[String] = RandomGen.lowerCaseString(5).replicateZIO(5).map(_.mkString(" "))

  def generate(personId: UUID)(
      id: Specified[UUID] = Specified.WasNotSpecified,
      note: Specified[String] = Specified.WasNotSpecified,
      note2: Specified[Option[String]] = Specified.WasNotSpecified,
  ): UIO[Note2] =
    for {
      id <- id.orGen { Random.nextUUID }
      note <- note.orGen { randomNote }
      note2 <- note2.orGen { randomNote.map(_.some) }
    } yield Note2(id, personId, note, note2)

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

final case class Nums(
    bi: BigInt,
    bd: BigDecimal,
    biOpt: Option[BigInt],
    bdOpt: Option[BigDecimal],
)
object Nums extends TableCompanion[Nums, Unit](TableRepr.derived[Nums])

final case class Arrays(
    _1: List[Int],
    _2: Set[String],
    _3: List[List[Boolean]],
    _4: Option[ArraySeq[String]],
)
object Arrays extends TableCompanion[Arrays, Unit](TableRepr.derived[Arrays])

final case class LTreeEx(
    @primaryKey id: UUID,
    tree: oxygen.sql.model.LTree,
)
object LTreeEx extends TableCompanion[LTreeEx, UUID](TableRepr.derived[LTreeEx])

@experimental
object queries {

  //////////////////////////////////////////////////////////////////////////////////////////////////////
  //      Insert
  //////////////////////////////////////////////////////////////////////////////////////////////////////

  @compile
  val insertNoteForPeople: Query =
    for {
      (_, into) <- Q.insert.fromSelect[Note]
      p <- Q.select[Person]
      _ <- into(
        Note(
          UUID.randomUUID(),
          p.id,
          mkSqlString(Q.const("Adding note for person "), p.first, " ", p.last, " : everyone"),
        ),
      )
    } yield ()

  @compile
  val insertNoteForPeople2: QueryI[(String, String)] =
    for {
      first <- Q.input[String]
      suffix <- Q.input[String]
      (_, into) <- Q.insert.fromSelect[Note]
      p <- Q.select[Person]
      _ <- where if p.first == first
      _ <- into(
        Note(
          UUID.randomUUID(),
          p.id,
          mkSqlString(Q.const("Adding note for person "), p.first, " ", p.last, " : ", suffix),
        ),
      )
    } yield ()

  @compile
  val insertNote2ForNote1: QueryI[UUID] =
    for {
      id <- Q.input[UUID]
      (_, into) <- Q.insert.fromSelect[Note2]
      n <- Q.select[Note2]
      p <- Q.join[Person] if p.id == n.personId
      _ <- where if n.id == id
      _ <- into(
        Note2(
          UUID.randomUUID(),
          p.id,
          mkSqlString(p.first, " ", p.last),
          Option(n.note),
        ),
      )
    } yield ()

  // OFF-328: `ON CONFLICT` (upsert) support in the hand-written DSL, targeting the primary key.

  @compile
  val insertPersonOnConflictDoNothing: QueryI[Person] =
    for {
      i <- input[Person]
      (_, into) <- Q.insert[Person]
      _ <- into(i).onConflictDoNothing
    } yield ()

  @compile
  val insertPersonOnConflictDoUpdate: QueryI[Person] =
    for {
      i <- input[Person]
      (_, into) <- Q.insert[Person]
      _ <- into(i).onConflictDoUpdate
    } yield ()

  @compile
  val insertNoteForPeopleOnConflictDoNothing: Query =
    for {
      (_, into) <- Q.insert.fromSelect[Note]
      p <- Q.select[Person]
      _ <- into(
        Note(
          UUID.randomUUID(),
          p.id,
          mkSqlString(Q.const("Adding note for person "), p.first, " ", p.last, " : everyone"),
        ),
      ).onConflictDoNothing
    } yield ()

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

  // OXY-17: IN / NOT IN -> static `col = ANY(?)` / `col <> ALL(?)` (single array bind)

  @compile
  val selectByIds: QueryIO[Seq[UUID], Person] =
    for {
      ids <- input[Seq[UUID]]
      p <- select[Person]
      _ <- where if p.id.in(ids)
    } yield p

  @compile
  val selectByIdsNotIn: QueryIO[Seq[UUID], Person] =
    for {
      ids <- input[Seq[UUID]]
      p <- select[Person]
      _ <- where if p.id.notIn(ids)
    } yield p

  // same as `selectByIds` / `selectByIdsNotIn`, but the value-list is a `Set` instead of a `Seq`
  @compile
  val selectByIdsSet: QueryIO[Set[UUID], Person] =
    for {
      ids <- input[Set[UUID]]
      p <- select[Person]
      _ <- where if p.id.in(ids)
    } yield p

  @compile
  val selectByIdsSetNotIn: QueryIO[Set[UUID], Person] =
    for {
      ids <- input[Set[UUID]]
      p <- select[Person]
      _ <- where if p.id.notIn(ids)
    } yield p

  @compile
  val selectByGroupAndIds: QueryIO[(UUID, Seq[UUID]), Person] =
    for {
      groupId <- input[UUID]
      ids <- input[Seq[UUID]]
      p <- select[Person]
      _ <- where if p.groupId == groupId && p.id.in(ids)
    } yield p

  @compile
  val selectByGroupAndIds2NotIn: QueryIO[(UUID, Seq[UUID]), Person] =
    for {
      groupId <- input[UUID]
      ids <- input[Seq[UUID]]
      p <- select[Person]
      _ <- where if p.groupId == groupId && p.id.notIn(ids)
    } yield p

  @compile
  val deleteByIds: QueryIO[Seq[UUID], Person] =
    for {
      ids <- input[Seq[UUID]]
      p <- delete[Person]
      _ <- where if p.id.in(ids)
    } yield p

  @compile
  val selectByIdArray: QueryIO[Seq[UUID], Person] =
    for {
      ids <- input.array[UUID]
      p <- select[Person]
      _ <- where if ids.contains(p.id)
    } yield p

  // same as `selectByIdArray`, but the input is a `Set` (`input.set`) instead of a `Seq`
  @compile
  val selectByIdSet: QueryIO[Set[UUID], Person] =
    for {
      ids <- input.set[UUID]
      p <- select[Person]
      _ <- where if ids.contains(p.id)
    } yield p

  @compile
  val selectByIdArrayAndGroup: QueryIO[(Seq[UUID], UUID), Person] =
    for {
      ids <- input.array[UUID]
      groupId <- input[UUID]
      p <- select[Person]
      _ <- where if ids.contains(p.id) && p.groupId == groupId
    } yield p

  @compile
  val personJoinNotesByIdArray: QueryIO[Seq[UUID], (Person, Note)] =
    for {
      ids <- input.array[UUID]
      p <- select[Person]
      n <- join[Note] if n.personId == p.id
      _ <- where if ids.contains(p.id)
    } yield (p, n)

  //////////////////////////////////////////////////////////////////////////////////////////////////////
  //      UNNEST as an input JOIN table source
  //////////////////////////////////////////////////////////////////////////////////////////////////////

  // SELECT ... FROM UNNEST(?::uuid[]) id JOIN note n ON n.person_id = id
  @compile
  val notesByPersonIdUnnest: QueryIO[Seq[UUID], Note] =
    for {
      ids <- input.array[UUID]
      id <- select.unnest(ids)
      n <- join[Note] if n.personId == id
    } yield n

  // UNNEST over a `Set` input (`input.set`) rather than a `Seq`
  @compile
  val notesByPersonIdUnnestSet: QueryIO[Set[UUID], Note] =
    for {
      ids <- input.set[UUID]
      id <- select.unnest(ids)
      n <- join[Note] if n.personId == id
    } yield n

  // also selects the unnested column itself, to prove it decodes + can be referenced in the SELECT
  @compile
  val personIdAndNoteUnnest: QueryIO[Seq[UUID], (UUID, Note)] =
    for {
      ids <- input.array[UUID]
      id <- select.unnest(ids)
      n <- join[Note] if n.personId == id
    } yield (id, n)

  // composition: UNNEST array input + an additional scalar input used in a WHERE
  @compile
  val notesByPersonIdUnnestFilteredByNote: QueryIO[(Seq[UUID], String), Note] =
    for {
      ids <- input.array[UUID]
      noteText <- input[String]
      id <- select.unnest(ids)
      n <- join[Note] if n.personId == id
      _ <- where if n.note == noteText
    } yield n

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
  val intsOrderByABOffsetOptional: QueryIO[(Option[Int], Option[Int]), Ints] =
    for {
      l <- input.optional[Int]
      o <- input.optional[Int]
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

  // scalar aggregates (OXY-166) over a group. NULL over an empty group -> None (except `sum`'s COALESCE variant -> 0).

  // `sum(_)` == `sum.orZero(_)` -> COALESCE(SUM(_), 0), non-optional (0 over an empty group).
  @compile
  val personAgeSumByGroup: QueryIO[UUID, Long] =
    for {
      groupId <- input[UUID]
      p <- select[Person]
      _ <- where if p.groupId == groupId
    } yield Q.sum(p.age)

  // `sum.orNull(_)` -> SUM(_), Option (None over an empty group).
  @compile
  val personAgeSumOrNullByGroup: QueryIO[UUID, Option[Long]] =
    for {
      groupId <- input[UUID]
      p <- select[Person]
      _ <- where if p.groupId == groupId
    } yield Q.sum.orNull(p.age)

  @compile
  val personAgeAvgByGroup: QueryIO[UUID, Option[BigDecimal]] =
    for {
      groupId <- input[UUID]
      p <- select[Person]
      _ <- where if p.groupId == groupId
    } yield Q.avg(p.age)

  @compile
  val personAgeMinByGroup: QueryIO[UUID, Option[Int]] =
    for {
      groupId <- input[UUID]
      p <- select[Person]
      _ <- where if p.groupId == groupId
    } yield Q.min(p.age)

  @compile
  val personAgeMaxByGroup: QueryIO[UUID, Option[Int]] =
    for {
      groupId <- input[UUID]
      p <- select[Person]
      _ <- where if p.groupId == groupId
    } yield Q.max(p.age)

  @compile
  val selectSubQuery1: QueryO[(Person, Option[Note])] =
    for {
      p <- Q.select.subQuery("sub1") {
        for {
          p <- Q.select[Person]
          _ <- Q.orderBy(p.first.asc)
          _ <- Q.limit(Q.const(2))
        } yield p
      }
      n <- Q.leftJoin[Note] if n.personId == p.id
    } yield (p, n)

  @compile
  val selectSubQuery2: QueryO[(Person, Option[Note])] =
    for {
      (p, n) <- Q.select.subQuery("sub1") {
        for {
          p <- Q.select[Person]
          n <- Q.leftJoin[Note] if n.personId == p.id
          _ <- Q.orderBy(p.first.asc)
        } yield (p, n)
      }
      _ <- limit(Q.const(4))
    } yield (p, n)

  @compile
  val selectIsEmpty: QueryO[Note2] =
    for {
      n <- Q.select[Note2]
      _ <- where if n.note2.isEmpty
    } yield n

  @compile
  val selectNonEmpty: QueryO[Note2] =
    for {
      n <- Q.select[Note2]
      _ <- where if n.note2.nonEmpty
    } yield n

  @compile
  val ltreeAncestors: QueryIO[oxygen.sql.model.LTree, LTreeEx] =
    for {
      in <- input[oxygen.sql.model.LTree]
      n <- Q.select[LTreeEx]
      _ <- where if n.tree @> in
    } yield n

  @compile
  val ltreeDescendants: QueryIO[oxygen.sql.model.LTree, LTreeEx] =
    for {
      in <- input[oxygen.sql.model.LTree]
      n <- Q.select[LTreeEx]
      _ <- where if n.tree <@ in
    } yield n

  @compile
  val personFirstLike: QueryIO[String, Person] =
    for {
      pattern <- input[String]
      p <- select[Person]
      _ <- where if p.first.like(pattern)
    } yield p

  @compile
  val personFirstILike: QueryIO[String, Person] =
    for {
      pattern <- input[String]
      p <- select[Person]
      _ <- where if p.first.ilike(pattern)
    } yield p

  @compile
  val personFirstNotLike: QueryIO[String, Person] =
    for {
      pattern <- input[String]
      p <- select[Person]
      _ <- where if p.first.notLike(pattern)
    } yield p

  @compile
  val personFirstNotILike: QueryIO[String, Person] =
    for {
      pattern <- input[String]
      p <- select[Person]
      _ <- where if p.first.notILike(pattern)
    } yield p

  // LIKE composed with another predicate (`&&`)
  @compile
  val personLikeAndMinAge: QueryIO[(String, Int), Person] =
    for {
      pattern <- input[String]
      minAge <- input[Int]
      p <- select[Person]
      _ <- where if p.first.like(pattern) && p.age >= minAge
    } yield p

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
  //      Delete
  //////////////////////////////////////////////////////////////////////////////////////////////////////

  @compile
  val deleteByGroupId: QueryIO[UUID, Person] =
    for {
      i <- input[UUID]
      p1 <- delete[Person]
      _ <- where if p1.groupId == i
    } yield p1

}
