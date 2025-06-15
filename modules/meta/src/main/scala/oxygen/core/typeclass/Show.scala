package oxygen.core.typeclass

import oxygen.meta2.*
import oxygen.predef.core.*
import oxygen.quoted.*
import scala.annotation.Annotation
import scala.collection.mutable
import scala.quoted.*

trait Show[A] {

  def show(value: A): String

  def writeTo(builder: mutable.StringBuilder, value: A): Unit = builder.append(show(value))

}
object Show extends K0.Derivable[Show] {

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

    final case class fieldName(name: String) extends Annotation
    object fieldName {

      given FromExpr[fieldName] =
        new FromExpr[fieldName] {
          override def unapply(x: Expr[fieldName])(using Quotes): Option[fieldName] = x match
            case '{ new `fieldName`(${ Expr(name) }) }   => fieldName(name).some
            case '{ `fieldName`(${ Expr(name) }) }       => fieldName(name).some
            case '{ `fieldName`.apply(${ Expr(name) }) } => fieldName(name).some
            case _                                       => None
        }

    }

    final case class typeName(name: String) extends Annotation
    object typeName {

      given FromExpr[typeName] =
        new FromExpr[typeName] {
          override def unapply(x: Expr[typeName])(using Quotes): Option[typeName] = x match
            case '{ new `typeName`(${ Expr(name) }) }   => typeName(name).some
            case '{ `typeName`(${ Expr(name) }) }       => typeName(name).some
            case '{ `typeName`.apply(${ Expr(name) }) } => typeName(name).some
            case _                                      => None
        }

    }

  }

  //////////////////////////////////////////////////////////////////////////////////////////////////////
  //      Generic
  //////////////////////////////////////////////////////////////////////////////////////////////////////

  override protected def productDeriver[A](using Quotes, Type[Show], Type[A], K0.ProductGeneric[A], K0.Derivable[Show]): K0.Derivable.ProductDeriver[Show, A] =
    K0.Derivable.ProductDeriver.withInstances[Show, A] { i =>
      new K0.Derivable.ProductDeriver.Split[Show, A] {

        private def makeWriteTo(builder: Expr[mutable.StringBuilder], value: Expr[A]): Expr[Unit] =
          generic.mapChildren
            .map[Option[Growable[StringExpr]]] {
              [b] =>
                _ ?=>
                  (field: generic.Field[b]) =>
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
                        StringExpr.const(field.annotations.optionalOfValue[annotation.fieldName].fold(field.name)(_.name)),
                        StringExpr.const(" = "),
                        value,
                      )
                  }
            }
            .flatMap(identity)
            .surround(
              Growable.single(StringExpr.const(generic.annotations.optionalOfValue[annotation.typeName].fold(generic.label)(_.name) + "(")),
              Growable.single(StringExpr.const(", ")),
              Growable.single(StringExpr.const(")")),
            )
            .flatten
            .exprMkStringTo(builder)

        override def deriveCaseClass(generic: K0.ProductGeneric.CaseClassGeneric[A]): Expr[Show[A]] =
          '{
            new PreferBuffer[A] {
              override def writeTo(builder: mutable.StringBuilder, value: A): Unit = ${ makeWriteTo('builder, 'value) }
            }
          }

        override def deriveAnyVal[B: Type](generic: K0.ProductGeneric.AnyValGeneric[A, B]): Expr[Show[A]] =
          '{
            new Show[A] {
              override def show(value: A): String =
                ${ generic.field.getExpr(i) }.show(${ generic.anyVal.unwrap('value) })
              override def writeTo(builder: mutable.StringBuilder, value: A): Unit =
                ${ generic.field.getExpr(i) }.writeTo(builder, ${ generic.anyVal.unwrap('value) })
            }
          }

        override def deriveCaseObject(generic: K0.ProductGeneric.CaseObjectGeneric[A]): Expr[Show[A]] =
          '{
            new Show[A] {
              override def show(value: A): String = ${ Expr(generic.annotations.optionalOfValue[annotation.typeName].fold(generic.label)(_.name)) }
            }
          }

      }
    }

  override protected def sumDeriver[A](using Quotes, Type[Show], Type[A], K0.SumGeneric[A], K0.Derivable[Show]): K0.Derivable.SumDeriver[Show, A] =
    K0.Derivable.SumDeriver.withInstances[Show, A] { _ =>
      new K0.Derivable.SumDeriver[Show, A] {

        override def derive: Expr[Show[A]] =
          '{
            new PreferBuffer[A] {
              override def writeTo(builder: mutable.StringBuilder, value: A): Unit = {
                builder.append(${ Expr(generic.annotations.optionalOfValue[annotation.typeName].fold(generic.label)(_.name) + ".") })
                builder.append("TODO : pick case")
              }
            }
          }

      }
    }

  inline def derived[A]: Show[A] = ${ derivedImpl[A] }

}
