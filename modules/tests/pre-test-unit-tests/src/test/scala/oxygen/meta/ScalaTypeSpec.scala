package oxygen.meta

// FIX-PRE-MERGE (KR) :
object Outer {

  opaque type InvisibleOpaqueType1 = Int
  opaque type InvisibleOpaqueType2 <: java.time.temporal.Temporal = java.time.LocalTime
  opaque type InvisibleOpaqueType3[A] = Option[A]

}

// FIX-PRE-MERGE (KR) : make an actual spec?
object ScalaTypeSpec {

  sealed trait SealedTrait1
  final case class CaseClass1() extends SealedTrait1
  case object CaseObject1 extends SealedTrait1

  sealed trait SealedTrait2[A]
  final case class CaseClass2[A]() extends SealedTrait2[A]
  final case class CaseClass3[A]() extends SealedTrait2[Option[A]]
  case object CaseObject2 extends SealedTrait2[Unit]

  final class SimpleClass
  final class WeirdClass1[A, B](val a: A)(val b: B)(c: A, d: B) {
    println((a, b, c, d))
  }

  type Alias1[A] = List
  type Alias2[A] = List[Option[A]]
  type Alias3[A] = [B] =>> Either[A, B]

  opaque type VisibleOpaqueType1 = Int
  opaque type VisibleOpaqueType2 <: java.time.temporal.Temporal = java.time.LocalTime
  opaque type VisibleOpaqueType3[A] = Option[A]

  //

  // Macros.doTheTypeStuff[Outer.InvisibleOpaqueType1]

}

// FIX-PRE-MERGE (KR) : remove

//////////////////////////////////////////////////////////////////////////////////////////////////////
//
//////////////////////////////////////////////////////////////////////////////////////////////////////

import scala.quoted.*
import scala.reflect.ClassTag

sealed trait MySchema[A] {

  val typeName: String
  def transform[B](ab: A => B, ba: B => A)(using ct: ClassTag[B]): MySchema[B]

}
object MySchema {

  case class flattenFields() extends scala.annotation.Annotation

  trait NonObjectLike[A] extends MySchema[A] {
    override final def transform[B](ab: A => B, ba: B => A)(using ct: ClassTag[B]): MySchema.NonObjectLike[B] = MySchema.TransformNonObjectLike(this, ct.runtimeClass.getName, ab, ba)
  }

  final case class TransformNonObjectLike[A, B](a: NonObjectLike[A], typeName: String, ab: A => B, ba: B => A) extends NonObjectLike[B]

  trait ObjectLike[A] extends MySchema[A] {
    lazy val fields: List[Field[?]]
    override final def transform[B](ab: A => B, ba: B => A)(using ct: ClassTag[B]): MySchema.ObjectLike[B] = MySchema.TransformObjectLike(this, ct.runtimeClass.getName, ab, ba)
  }

  final case class TransformObjectLike[A, B](a: ObjectLike[A], typeName: String, ab: A => B, ba: B => A) extends ObjectLike[B] {
    override lazy val fields: List[Field[?]] = a.fields
  }

  final case class Field[B](
      name: String,
      schema: MySchema[B],
  )

  private def derivedImpl[A: Type](using Quotes): Expr[MySchema[A]] = ???

  inline def derived[A]: MySchema[A] = ${ derivedImpl[A] }

}

trait CaseClass[A] {

  val name: String
  val tpe: Type[A]
  val fields: List[Field[?]]

  def instantiate(exprs: List[Expr[?]]): Expr[A]

  def mapFields[O](f: [b] => Type[b] ?=> Field[b] => O): List[O] =
    fields.map { _field =>
      type B
      val field: Field[B] = _field.asInstanceOf[Field[B]]
      f[B](using field.tpe)(field)
    }

  def optionalAnnotationExpr[T <: scala.annotation.Annotation: Type](using Quotes): Option[Expr[T]]

  trait Field[B] {

    val name: String
    val tpe: Type[B]

    def select(expr: Expr[A])(using Quotes): Expr[B]
    def summonInstance[F[_]: Type](using Quotes): Expr[F[B]]

    def optionalAnnotationExpr[T <: scala.annotation.Annotation: Type](using Quotes): Option[Expr[T]]

  }

}
object CaseClass {

  def of[A: Type](using Quotes): CaseClass[A] = ???

}

object Usage {

  ///////  ///////////////////////////////////////////////////////////////

  def deriveCaseClass[A: Type](using Quotes): Expr[MySchema.ObjectLike[A]] = {
    val caseClass: CaseClass[A] = CaseClass.of[A]

    val childFieldExprs1: List[Expr[List[MySchema.Field[?]]]] =
      caseClass.mapFields[Expr[List[MySchema.Field[?]]]] { [b] => _ ?=> (field: caseClass.Field[b]) =>
        val flatten: Boolean = field.optionalAnnotationExpr[MySchema.flattenFields].nonEmpty
        val instance: Expr[MySchema[b]] = field.summonInstance[MySchema]
        // Yes, I summoned `MySchema[b]` instead of conditionally summoning `MySchema.ObjectLike[b]`. I will explain why later.

        if flatten then generateNestedField[A, b](caseClass)(field, instance)
        else generateFlattenedFields[A, b](caseClass)(field, instance)
      }

    val childFieldExprs2: Expr[List[MySchema.Field[?]]] =
      '{ ${ Expr.ofList(childFieldExprs1) }.flatten }

    '{
      new MySchema.ObjectLike[A] {
        override val typeName: String = ${ Expr(caseClass.name) }
        override lazy val fields: List[MySchema.Field[?]] = $childFieldExprs2
      }
    }
  }

  private def generateNestedField[A: Type, B: Type](caseClass: CaseClass[A])(field: caseClass.Field[B], instanceExpr: Expr[MySchema[B]])(using Quotes): Expr[List[MySchema.Field[B]]] =
    '{
      List(
        MySchema.Field[B](
          name = ${ Expr(field.name) },
          schema = $instanceExpr,
        ),
      )
    }

  private def generateFlattenedFields[A: Type, B: Type](caseClass: CaseClass[A])(field: caseClass.Field[B], instanceExpr: Expr[MySchema[B]])(using Quotes): Expr[List[MySchema.Field[?]]] =
    '{
      val instance: MySchema[B] = $instanceExpr
      instance match
        case instance: MySchema.ObjectLike[B] => instance.fields
        case _: MySchema.NonObjectLike[B]     => throw new RuntimeException(s"Can not flatten `MySchema.NonObjectLike` for field ${${ Expr(field.name) }}")
    }

  ///////  ///////////////////////////////////////////////////////////////

  private def allFieldNames(prefix: String, schema: MySchema[?]): List[String] =
    schema match {
      case _: MySchema.NonObjectLike[?]   => prefix :: Nil
      case schema: MySchema.ObjectLike[?] =>
        schema.fields.flatMap { field =>
          val newPrefix = if prefix.isEmpty then field.name else s"${prefix}_${field.name}"
          allFieldNames(newPrefix, field.schema)
        }
    }

  def sqlSelectAllQuery(schema: MySchema[?]): String =
    s"SELECT ${allFieldNames("", schema).mkString(", ")} FROM ${schema.typeName}"

  def conflictingFieldNames(schema: MySchema[?]): Set[String] = {
    val allFields: List[String] = allFieldNames("", schema)

    allFields.groupBy(identity)
      .iterator
      .flatMap { case (k, vs) => Option.when(vs.size != 1)(k) }
      .toSet
  }

  def sqlSelectAllQueryCompileTime(schema: Expr[MySchema[?]])(using Quotes): Expr[String] = '{ sqlSelectAllQuery($schema) }
  def conflictingFieldNamesCompileTime(schema: Expr[MySchema[?]])(using Quotes): Expr[Set[String]] = '{ conflictingFieldNames($schema) }

}

object MyTypes {

  given MySchema[Int] = ???
  given MySchema[String] = ???
  given MySchema[Boolean] = ???

  final case class CaseClass1(int: Int) derives MySchema

  final case class CaseClass2(string1: String, string2: String)
  object CaseClass2 {

    final case class Repr(string: String) derives MySchema

    private val reg = "^([^:]*):([^:]*)$".r

    given MySchema[CaseClass2] =
      summon[MySchema[CaseClass2.Repr]].transform(
        _.string match {
          case reg(a, b) => CaseClass2(a, b)
          case _         => ???
        },
        cc2 => CaseClass2.Repr(s"${cc2.string1}:${cc2.string2}"),
      )

  }

  final case class CaseClass3(cc1: CaseClass1, cc2: CaseClass2, boolean: Boolean) derives MySchema

  final case class CaseClass4(cc1: CaseClass1, @MySchema.flattenFields cc2: CaseClass2, boolean: Boolean) derives MySchema

}

object SimpleExample {

  object compiler {
    def compileTimeEval[A](expr: Expr[A])(using Quotes): A = ???
  }

  trait Tags[A] {
    def tags: Set[String]
  }
  object Tags {

    def const[A](t: String*): Tags[A] =
      new Tags[A] {
        override val tags: Set[String] = t.toSet
      }

    final case class Both[A, B](a: Tags[A], b: Tags[B]) extends Tags[A & B] {
      override lazy val tags: Set[String] = a.tags ++ b.tags
    }

    private def tagsForBothImpl[A: Type, B: Type](using quotes: Quotes): Expr[Tags[A & B]] = {
      import quotes.reflect.*
      val aInstanceExpr: Expr[Tags[A]] = Expr.summon[Tags[A]].getOrElse { report.errorAndAbort(s"Missing Tags for ${Type.show[A]}") }
      val bInstanceExpr: Expr[Tags[B]] = Expr.summon[Tags[B]].getOrElse { report.errorAndAbort(s"Missing Tags for ${Type.show[B]}") }
      val aInstance: Tags[A] = compiler.compileTimeEval { aInstanceExpr }
      val bInstance: Tags[B] = compiler.compileTimeEval { bInstanceExpr }
      val overlap: Set[String] = aInstance.tags & bInstance.tags
      if overlap.nonEmpty then
        report.errorAndAbort(
          s"""Overlap between Tags for ${Type.show[A]} & ${Type.show[B]}
           |  ${Type.show[A]} : ${aInstance.tags.mkString(", ")}
           |  ${Type.show[B]} : ${bInstance.tags.mkString(", ")}
           |  overlap: ${overlap.mkString(", ")}
           |""".stripMargin,
        )

      // here, you could technically choose to just inline a call to `Tags.const`,
      // but I arbitrarily chose to keep the original Expr[A]/Expr[B].
      '{ Tags.Both($aInstanceExpr, $bInstanceExpr) }
    }
    inline def tagsForBoth[A, B]: Tags[A & B] = ${ tagsForBothImpl[A, B] }

  }

  final class ClassA
  final class ClassB
  final class ClassC

  given aTags: Tags[ClassA] = Tags.const("A", "extra")
  given bTags: Tags[ClassB] = Tags.const("B")
  given cTags: Tags[ClassC] = Tags.const("C", "extra")

  val ab: Tags[ClassA & ClassB] = Tags.tagsForBoth[ClassA, ClassB] // inlines: Tags.Both(aTags, bTags)
  val bc: Tags[ClassB & ClassC] = Tags.tagsForBoth[ClassB, ClassC] // inlines: Tags.Both(bTags, cTags)
  val ac: Tags[ClassA & ClassC] = Tags.tagsForBoth[ClassA, ClassC] // compile error, overlap: extra

}
