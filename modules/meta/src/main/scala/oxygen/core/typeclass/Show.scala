package oxygen.core.typeclass

import oxygen.meta2.*
import oxygen.meta2.K0.ProductGeneric
import oxygen.predef.core.*
import oxygen.quoted.*
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

      given FromExpr[obfuscate] =
        new FromExpr[obfuscate] {
          override def unapply(x: Expr[obfuscate])(using Quotes): Option[obfuscate] = x match
            case '{ new `map`(${ Expr(name) }) }     => map(name).some
            case '{ `map`(${ Expr(name) }) }         => map(name).some
            case '{ `map`.apply(${ Expr(name) }) }   => map(name).some
            case '{ new `const`(${ Expr(name) }) }   => const(name).some
            case '{ `const`(${ Expr(name) }) }       => const(name).some
            case '{ `const`.apply(${ Expr(name) }) } => const(name).some
            case _                                   => None
        }

    }

    final class hide extends Annotation
    object hide {

      given FromExpr[hide] =
        new FromExpr[hide] {
          override def unapply(x: Expr[hide])(using Quotes): Option[hide] = x match
            case '{ new `hide` }     => hide().some
            case '{ new `hide`() }   => hide().some
            case '{ `hide`() }       => hide().some
            case '{ `hide`.apply() } => hide().some
            case _                   => None
        }

    }

  }

  //////////////////////////////////////////////////////////////////////////////////////////////////////
  //      Generic
  //////////////////////////////////////////////////////////////////////////////////////////////////////

  override protected def deriveProductImplInst[A](g: K0.ProductGeneric[A])(i: g.Expressions[Show])(using Quotes, Type[Show], Type[A]): Expr[Show[A]] = {
    def makeWriteTo(builder: Expr[mutable.StringBuilder], value: Expr[A]): Expr[Unit] = {
      g.util
        .map[Option[Growable[StringExpr]]] {
          [b] =>
            _ ?=>
              (field: g.Field[b]) =>
                val valueExpr: Option[StringExpr] =
                  (field.annotations.optionalOfValue[Show.annotation.obfuscate], field.annotations.optionalOfValue[Show.annotation.hide]) match {
                    case (None, None) =>
                      StringExpr.StrBuilder { builder => '{ ${ field.getExpr(i) }.writeTo($builder, ${ field.get(value) }) } }.some
                    case (Some(Show.annotation.obfuscate.map(char)), None) =>
                      StringExpr.StrBuilder { builder => '{ $builder.append(${ field.getExpr(i) }.show(${ field.get(value) }).map { _ => ${ Expr(char) } }) } }.some
                    case (Some(Show.annotation.obfuscate.const(str)), None) =>
                      StringExpr.const(str).some
                    case (None, Some(_))    => None
                    case (Some(_), Some(_)) => report.errorAndAbort("You can not both hide and obfuscate a field")
                  }

                valueExpr.map { value =>
                  Growable(
                    StringExpr.const(field.name),
                    StringExpr.const(" = "),
                    value,
                  )
              }
        }
        .flatMap(identity)
        .surround(
          Growable.single(StringExpr.const(g.label + "(")),
          Growable.single(StringExpr.const(", ")),
          Growable.single(StringExpr.const(")")),
        )
        .flatten
        .exprMkStringTo(builder)
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
