package oxygen.sql

import java.util.UUID
import oxygen.core.collection.Contiguous
import oxygen.sql.query.*
import oxygen.sql.query.dsl.*
import oxygen.sql.query.dsl.Q.*
import oxygen.sql.schema.*
import scala.annotation.experimental

// FIX-PRE-MERGE (KR) : move and make tests
@experimental
object queries {

  final case class Person(
      @primaryKey id: UUID,
      first: String,
      last: String,
  )
  object Person {
    given TableRepr[Person, UUID] = TableRepr.derived
  }

  //////////////////////////////////////////////////////////////////////////////////////////////////////
  //      Insert
  //////////////////////////////////////////////////////////////////////////////////////////////////////

  // @compile
  val insert1: QueryI[Person] =
    for {
      inputPerson <- input[Person]
      (_, into) <- insert[Person]
      _ <- into(inputPerson)
    } yield ()

  @compile
  val insert2: QueryIO[Person, UUID] =
    for {
      inputPerson <- input[Person]
      (p, into) <- insert[Person]
      _ <- into(inputPerson)
    } yield p.id

  @compile
  val insert3: QueryIO[Person, UUID] =
    for {
      _ <- input[Person]
      (p, _) <- insert[Person]
    } yield p.id

  //////////////////////////////////////////////////////////////////////////////////////////////////////
  //      Select
  //////////////////////////////////////////////////////////////////////////////////////////////////////

  // @compile
  val select1: QueryO[Person] =
    for {
      p1 <- select[Person]
    } yield p1

  // @compile
  val select2: QueryIO[String, Person] =
    for {
      f <- input[String]
      p1 <- select[Person]
      _ <- where if p1.first == f
    } yield p1

  // @compile
  val select3: QueryIO[(String, String), Person] =
    for {
      f <- input[String]
      l <- input[String]
      p1 <- select[Person]
      _ <- where if p1.first == f && p1.last == l
    } yield p1

  // @compile
  val select4: QueryO[Person] =
    for {
      f <- input.const("first")
      p1 <- select[Person]
      _ <- where if p1.first == f
    } yield p1

  // @compile
  val select5: QueryIO[String, Person] =
    for {
      f <- input[String]
      l <- input.const("last")
      p1 <- select[Person]
      _ <- where if p1.first == f && p1.last == l
    } yield p1

  //////////////////////////////////////////////////////////////////////////////////////////////////////
  //      Update
  //////////////////////////////////////////////////////////////////////////////////////////////////////

  // @compile
  val update1: Query =
    for {
      f <- input.const("first")
      (_, set) <- update[Person]
      _ <- set(_.first := f)
    } yield ()

  // @compile
  val update2: QueryI[String] =
    for {
      f <- input[String]
      (_, set) <- update[Person]
      _ <- set(_.first := f)
    } yield ()

  // @compile
  val update3: QueryO[UUID] =
    for {
      f <- input.const("first")
      (p, set) <- update[Person]
      _ <- set(_.first := f)
    } yield p.id

  // @compile
  val update4: QueryIO[String, UUID] =
    for {
      f <- input[String]
      (p, set) <- update[Person]
      _ <- set(_.first := f)
    } yield p.id

  //////////////////////////////////////////////////////////////////////////////////////////////////////
  //      Delete
  //////////////////////////////////////////////////////////////////////////////////////////////////////

  // @compile
  val delete1: Query =
    for {
      _ <- delete[Person]
    } yield ()

  // @compile
  val delete2: QueryI[UUID] =
    for {
      i <- input[UUID]
      p <- delete[Person]
      _ <- where if p.id == i
    } yield ()

  // @compile
  val delete3: QueryO[Person] =
    for {
      i <- input.const(UUID.randomUUID)
      p <- delete[Person]
      _ <- where if p.id == i
    } yield p

  // @compile
  val delete4: QueryIO[UUID, Person] =
    for {
      i <- input[UUID]
      p <- delete[Person]
      _ <- where if p.id == i
    } yield p

  //////////////////////////////////////////////////////////////////////////////////////////////////////
  //      Joins
  //////////////////////////////////////////////////////////////////////////////////////////////////////

  // @compile
  val join1: QueryIO[UUID, (Person, Person)] =
    for {
      i <- input[UUID]
      p1 <- select[Person]
      p2 <- join[Person] if p2.last == p1.last
      _ <- where if p1.id == i
    } yield (p1, p2)

  // @compile
  val join2: QueryIO[UUID, (Person, Option[Person])] =
    for {
      i <- input[UUID]
      p1 <- select[Person]
      p2 <- leftJoin[Person] if p2.last == p1.last
      _ <- where if p1.id == i
    } yield (p1, p2)

  // @compile
  val getWithLastName: QueryIO[String, Person] =
    for {
      l <- input[String]
      p <- select[Person]
      _ <- where if p.last == l
    } yield p

  // @compile
  val join3: QueryIO[UUID, (Person, Contiguous[Person])] =
    for {
      i <- input[UUID]
      p1 <- select[Person]
      _ <- where if p1.id == i
    } yield (p1, getWithLastName.subQueryAll(p1.last))

}
