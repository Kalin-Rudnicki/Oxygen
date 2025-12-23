package oxygen.meta.exprEvaluator

import oxygen.meta.*
import oxygen.meta.k0.*
import oxygen.predef.core.*
import oxygen.quoted.*
import scala.quoted.*

final case class SqlColumn(
    columnName: String,
    columnType: String,
    nullable: Boolean,
)

trait SqlSchema[A] {

  val columns: List[SqlColumn]
  final lazy val size: Int = columns.size

  def encodeInternal(value: A): Growable[Any]
  final def encode(value: A): ArraySeq[Any] = encodeInternal(value).toArraySeq

  def decodeInternal(values: ArraySeq[Matchable]): Either[String, A]
  final def decode(values: ArraySeq[Any]): Either[String, A] =
    if values.length != this.size then s"Invalid size [expected=${this.size}] [actual=${values.length}]".asLeft
    else decodeInternal(values.asInstanceOf[ArraySeq[Matchable]])

}
object SqlSchema {

  /////// Givens ///////////////////////////////////////////////////////////////

  private def primitive[A](tpe: String)(fromColumn: PartialFunction[Matchable, A]): SqlSchema.Primitive[A] =
    SqlSchema.Primitive(tpe, fromColumn)

  given int: SqlSchema[Int] = primitive[Int]("INT") { case v: Int => v }
  given string: SqlSchema[String] = primitive[String]("TEXT") { case v: String => v }
  given boolean: SqlSchema[Boolean] = primitive[Boolean]("BOOLEAN") { case v: Boolean => v }

  given option: [A: SqlSchema as underlying] => SqlSchema[Option[A]] = SqlSchema.Optional(underlying)

  /////// Instances ///////////////////////////////////////////////////////////////

  final case class Primitive[A](tpe: String, fromColumn: PartialFunction[Matchable, A]) extends SqlSchema[A] {
    private val fromColumnLifted: Matchable => Option[A] = fromColumn.lift
    override val columns: List[SqlColumn] = SqlColumn("", tpe, false) :: Nil
    override def encodeInternal(value: A): Growable[Any] = Growable.single(value)
    override def decodeInternal(values: ArraySeq[Matchable]): Either[String, A] = {
      val value: Matchable = values.head
      fromColumnLifted(value).toRight(s"Invalid $tpe : $value")
    }

  }

  final case class Optional[A](underlying: SqlSchema[A]) extends SqlSchema[Option[A]] {
    override val columns: List[SqlColumn] = underlying.columns.map(_.copy(nullable = true))
    override def encodeInternal(value: Option[A]): Growable[Any] = value match
      case Some(value) => underlying.encodeInternal(value)
      case None        => Growable.fill(underlying.size)(null)
    override def decodeInternal(values: ArraySeq[Matchable]): Either[String, Option[A]] =
      if values.forall { _ == null } then None.asRight
      else underlying.decodeInternal(values).map(_.some)
  }

  /////// Derived ///////////////////////////////////////////////////////////////

  object internal {

    private def columnsExpr[A: Type](gen: ProductGeneric[A], instances: Expressions[SqlSchema, A])(using Quotes): Expr[List[SqlColumn]] = {
      val tmp: Growable[Expr[List[SqlColumn]]] =
        gen.mapChildren.mapExpr { [b] => (_, _) ?=> (field: gen.Field[b]) =>
          '{ ${ field.getExpr(instances) }.columns }
        }

      '{ ${ tmp.seqToExprOf[List] }.flatten }
    }

    private def encodeInternalExpr[A: Type](gen: ProductGeneric[A], instances: Expressions[SqlSchema, A], valueExpr: Expr[A])(using Quotes): Expr[Growable[Any]] = {
      val tmp: Growable[Expr[Growable[Any]]] =
        gen.mapChildren.mapExpr { [b] => (_, _) ?=> (field: gen.Field[b]) =>
          '{ ${ field.getExpr(instances) }.encodeInternal(${ field.fromParent(valueExpr) }) }
        }

      '{ ${ tmp.seqToExpr }.flatten }
    }

    private def decodeInternalExpr[A: Type](
        gen: ProductGeneric[A],
        instances: Expressions[SqlSchema, A],
        offsets: Expressions[Const[(Int, Int)], A],
        valuesExpr: Expr[ArraySeq[Matchable]],
    )(using Quotes): Expr[Either[String, A]] =
      gen.instantiate.either[String] { [b] => (_, _) ?=> (field: gen.Field[b]) =>
        val myStartExpr: Expr[Int] = '{ ${ field.getExpr(offsets) }._1 }
        val myEndExpr: Expr[Int] = '{ ${ field.getExpr(offsets) }._2 }
        val myInstExpr: Expr[SqlSchema[b]] = field.getExpr(instances)
        val myValuesExpr: Expr[ArraySeq[Matchable]] = '{ $valuesExpr.slice($myStartExpr, $myEndExpr) }

        '{ $myInstExpr.decodeInternal($myValuesExpr) }
      }

    private def makeOffsets[A: Type](gen: ProductGeneric.CaseClassGeneric[A], instances: Expressions[SqlSchema, A])(using Quotes): ValDefinitions[Const[(Int, Int)], A] =
      gen.cacheVals.foldLeft[(Int, Int)]("offset_" + _, ValDef.ValType.LazyVal)('{ (0, 0) }) { [b] => (_, _) ?=> (field: gen.Field[b], acc: Expr[(Int, Int)]) =>
        '{
          val (_, prevEnd): (Int, Int) = $acc
          val myInst: SqlSchema[b] = ${ field.getExpr(instances) }
          (prevEnd, prevEnd + myInst.size)
        }
      }

    def derivedImpl[A: Type](gen: ProductGeneric.CaseClassGeneric[A])(using Quotes): Expr[SqlSchema[A]] = {
      val valDefinitions: ValDefinitions[SqlSchema, A] = gen.cacheVals.summonTypeClasses[SqlSchema]()
      valDefinitions.defineAndUse { instances =>
        makeOffsets[A](gen, instances).defineAndUse { offsets =>
          '{
            new SqlSchema[A] {

              override val columns: List[SqlColumn] = ${ columnsExpr[A](gen, instances) }

              override def encodeInternal(value: A): Growable[Any] = ${ encodeInternalExpr[A](gen, instances, 'value) }

              override def decodeInternal(values: ArraySeq[Matchable]): Either[String, A] = ${ decodeInternalExpr[A](gen, instances, offsets, 'values) }

            }
          }
        }
      }
    }

    def derivedImpl[A: Type](using Quotes): Expr[SqlSchema[A]] =
      derivedImpl[A](ProductGeneric.CaseClassGeneric.of[A])

  }

  inline def derived[A]: SqlSchema[A] = ${ internal.derivedImpl[A] }

}

trait SqlTable[A] {
  val tableName: String
  val schema: SqlSchema[A]
}
object SqlTable {

  def derivedImpl[A: Type](using Quotes): Expr[SqlTable[A]] = {
    val gen: ProductGeneric.CaseClassGeneric[A] = ProductGeneric.CaseClassGeneric.of[A]
    val schemaExpr: Expr[SqlSchema[A]] = SqlSchema.internal.derivedImpl[A](gen)
    val expr: Expr[SqlTable[A]] =
      '{
        new SqlTable[A] {
          override val tableName: String = ${ Expr(gen.name) }
          override val schema: SqlSchema[A] = $schemaExpr
        }
      }

    report.info(expr.showAnsiCode)

    expr
  }

  inline def derived[A]: SqlTable[A] = ${ derivedImpl[A] }

}
