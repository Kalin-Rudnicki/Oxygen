package oxygen.core.typeclass

import oxygen.meta2.*
import oxygen.meta2.K0.ProductGeneric
import oxygen.predef.core.*
import scala.annotation.Annotation
import scala.collection.mutable
import scala.quoted.*

trait Show[A] {

  def show(value: A): String

  def writeTo(builder: mutable.StringBuilder, value: A): Unit = builder.append(show(value))

}
object Show extends K0.Derivable.WithInstances[Show], K0.Derivable.WrapAnyVal[Show] {

  inline def apply[A: Show as ev]: Show[A] = ev

  trait PreferBuffer[A] extends Show[A] {

    override def show(value: A): String = {
      val builder = mutable.StringBuilder()
      writeTo(builder, value)
      builder.result()
    }

    override def writeTo(builder: mutable.StringBuilder, value: A): Unit

  }

  def usingToString[A]: Show[A] = _.toString

  given string: Show[String] = _.unesc
  given boolean: Show[Boolean] = usingToString
  given byte: Show[Byte] = usingToString
  given short: Show[Short] = usingToString
  given int: Show[Int] = usingToString
  given long: Show[Long] = usingToString
  given bigInt: Show[BigInt] = usingToString
  given float: Show[Float] = usingToString
  given double: Show[Double] = usingToString
  given bigDecimal: Show[BigDecimal] = usingToString

  def showOption[A: Show as showA](
      somePrefix: String,
      someSuffix: String,
      none: String,
  ): Show[Option[A]] =
    new PreferBuffer[Option[A]] {
      override def writeTo(builder: mutable.StringBuilder, value: Option[A]): Unit =
        value match {
          case Some(value) =>
            builder.append(somePrefix)
            showA.writeTo(builder, value)
            builder.append(someSuffix)
          case None =>
            builder.append(none)
        }
    }

  given option: [A: Show as showA] => Show[Option[A]] =
    showOption("", "", "<none>")

  def showSeq[S[_]: SeqOps as seqOps, A: Show as showA](
      prefix: String,
      join: String,
      suffix: String,
  ): Show[S[A]] =
    new PreferBuffer[S[A]] {
      override def writeTo(builder: mutable.StringBuilder, value: S[A]): Unit = {
        val iter = seqOps.newIterator(value)
        builder.append(prefix)
        iter.nextOption() match {
          case Some(value) =>
            showA.writeTo(builder, value)
          case None =>
        }
        while (iter.hasNext) {
          builder.append(join)
          showA.writeTo(builder, iter.next())
        }
        builder.append(suffix)
      }
    }

  given seq: [S[_]: SeqOps as seqOps, A: Show as showA] => Show[S[A]] =
    showSeq("[", ", ", "]")

  object annotation {

    sealed trait obfuscate extends Annotation
    object obfuscate {
      final case class map(char: Char) extends obfuscate
      final case class const(str: String) extends obfuscate
    }

    final class hide extends Annotation

  }

  //////////////////////////////////////////////////////////////////////////////////////////////////////
  //      Generic
  //////////////////////////////////////////////////////////////////////////////////////////////////////

  override protected def deriveProductImplInst[A](g: K0.ProductGeneric[A])(i: g.Expressions[Show])(using Quotes, Type[Show], Type[A]): Expr[Show[A]] = {
    def makeWriteTo(builder: Expr[mutable.StringBuilder], value: Expr[A]): Expr[Unit] = {
      val strExprs: Growable[StringExpr] =
        g.util.flatMapJoin[Contiguous, StringExpr](
          StringExpr.const(g.label + "("),
          StringExpr.const(", "),
          StringExpr.const(")"),
        ) {
          [b] =>
            _ ?=>
              (field: g.Field[b]) =>
                Contiguous(
                  StringExpr.const(field.name),
                  StringExpr.const(" = "),
                  StringExpr.StrBuilder { builder => '{ ${ field.getExpr(i) }.writeTo($builder, ${ field.get(value) }) } },
              )
        }

      val res = strExprs.exprMkStringTo(builder)

      // report.errorAndAbort(res.show)

      res
    }

    '{
      new PreferBuffer[A] {
        override def writeTo(builder: mutable.StringBuilder, value: A): Unit = ${ makeWriteTo('builder, 'value) }
      }
    }
  }

  override protected def wrapAnyValInstance[A, B](a: Expr[Show[A]], wrap: Expr[A] => Expr[B], unwrap: Expr[B] => Expr[A])(using Quotes, Type[A], Type[B]): Expr[Show[B]] =
    '{
      new Show[B] {
        override def show(value: B): String =
          $a.show(${ unwrap('value) })
        override def writeTo(builder: mutable.StringBuilder, value: B): Unit =
          $a.writeTo(builder, ${ unwrap('value) })
      }
    }

  override protected def deriveCaseObjectImpl[A](g: ProductGeneric.CaseObjectGeneric[A])(using Quotes, Type[Show], Type[A]): Expr[Show[A]] =
    '{
      new Show[A] {
        override def show(value: A): String = ${ Expr(g.label) }
      }
    }

  override protected def deriveSumImplInst[A](g: K0.SumGeneric[A])(i: g.Expressions[Show])(using Quotes, Type[Show], Type[A]): Expr[Show[A]] =
    ??? // FIX-PRE-MERGE (KR) :

  inline def derived[A]: Show[A] = ${ derivedImpl[A] }

}
