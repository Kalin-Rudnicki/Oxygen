package oxygen.core.typeclass

import java.time.*
import java.util.{TimeZone, UUID}
import oxygen.core.*
import oxygen.core.collection.*
import oxygen.core.syntax.common.*
import oxygen.meta.{*, given}
import oxygen.meta.k0.*
import oxygen.quoted.*
import scala.annotation.Annotation
import scala.collection.mutable
import scala.quoted.*

trait Show[A] {

  def show(value: A): String

  def writeTo(builder: mutable.StringBuilder, value: A): Unit = builder.append(show(value))

}
object Show extends ShowLowPriority.LowPriority1, Derivable[Show] {

  inline def apply[A: Show as ev]: Show[A] = ev

  trait PreferBuffer[A] extends Show[A] {

    override def show(value: A): String = {
      val builder = mutable.StringBuilder()
      writeTo(builder, value)
      builder.result()
    }

    override def writeTo(builder: mutable.StringBuilder, value: A): Unit

  }

  final class ToString[A] extends Show[A] {
    override def show(value: A): String = value.toString
  }
  object ToString {
    inline def derived[A]: Show.ToString[A] = new ToString[A]
  }

  def usingToString[A]: Show[A] = new ToString[A]

  def shown[A](f: A => String): Show[A] = f(_)

  given string: Show[String] = _.unesc
  given boolean: Show[Boolean] = usingToString
  given uuid: Show[UUID] = usingToString
  given byte: Show[Byte] = usingToString
  given short: Show[Short] = usingToString
  given int: Show[Int] = usingToString
  given long: Show[Long] = usingToString
  given bigInt: Show[BigInt] = usingToString
  given float: Show[Float] = usingToString
  given double: Show[Double] = usingToString
  given bigDecimal: Show[BigDecimal] = usingToString

  given localTime: Show[LocalTime] = usingToString
  given localDate: Show[LocalDate] = usingToString
  given localDateTime: Show[LocalDateTime] = usingToString
  given zonedDateTime: Show[ZonedDateTime] = usingToString
  given offsetDateTime: Show[OffsetDateTime] = usingToString
  given offsetTime: Show[OffsetTime] = usingToString
  given instant: Show[Instant] = usingToString
  given duration: Show[Duration] = usingToString
  given period: Show[Period] = usingToString
  given zoneId: Show[ZoneId] = usingToString
  given zoneOffset: Show[ZoneOffset] = usingToString
  given timeZone: Show[TimeZone] = usingToString
  given monthDay: Show[MonthDay] = usingToString
  given year: Show[Year] = usingToString
  given yearMonth: Show[YearMonth] = usingToString
  given month: Show[Month] = usingToString

  given throwable: Show[Throwable] = _.safeGetMessage

  given strictEnum: [A: StrictEnum as e] => Show[A] = e.encode(_)
  given enumWithOther: [A: EnumWithOther as e] => Show[A] = e.encode(_)

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

  def showSpecified[A: Show as showA](
      specifiedPrefix: String,
      specifiedSuffix: String,
      unspecified: String,
  ): Show[Specified[A]] =
    new PreferBuffer[Specified[A]] {
      override def writeTo(builder: mutable.StringBuilder, value: Specified[A]): Unit =
        value match {
          case Specified.WasSpecified(value) =>
            builder.append(specifiedPrefix)
            showA.writeTo(builder, value)
            builder.append(specifiedSuffix)
          case Specified.WasNotSpecified =>
            builder.append(unspecified)
        }
    }

  given specified: [A: Show as showA] => Show[Specified[A]] =
    showSpecified("", "", "<unspecified>")

  def showSeq[S[_]: SeqRead as seqOps, A: Show as showA](
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
        while iter.hasNext do {
          builder.append(join)
          showA.writeTo(builder, iter.next())
        }
        builder.append(suffix)
      }
    }

  final case class Shown(value: String) {
    override def toString: String = value
  }
  object Shown {

    def show[A: Show as showA](value: A): Shown = Shown(showA.show(value))

    given showableToShown: [A: Show as s] => Conversion[A, Shown] = a => Shown(s.show(a))

  }

  final case class ValueWithShown[+A](value: A, shown: Lazy[Shown]) {
    override def toString: String = shown.value.value
  }
  object ValueWithShown {

    def show[A: Show](value: A): ValueWithShown[A] = ValueWithShown[A](value, Lazy { Shown.show(value) })

    given valueWithShownToShown: [A: Show as s] => Conversion[A, ValueWithShown[A]] = ValueWithShown.show(_)

  }

  //////////////////////////////////////////////////////////////////////////////////////////////////////
  //      Generic
  //////////////////////////////////////////////////////////////////////////////////////////////////////

  object annotation {

    sealed trait obfuscate extends Annotation derives FromExprT
    object obfuscate {

      final case class map(char: Char) extends obfuscate
      final case class const(str: String) extends obfuscate

    }

    final case class hide() extends Annotation derives FromExprT

    final case class fieldName(name: String) extends Annotation derives FromExprT

    final case class typeName(name: String) extends Annotation derives FromExprT

  }

  override protected def productDeriver[A](using Quotes, Type[Show], Type[A], ProductGeneric[A], Derivable[Show]): Derivable.ProductDeriver[Show, A] =
    Derivable.ProductDeriver.withInstances[Show, A] { instances =>
      new Derivable.ProductDeriver.Split[Show, A] {

        private def makeWriteTo(builder: Expr[mutable.StringBuilder], value: Expr[A]): Expr[Unit] =
          generic.mapChildren
            .map[Option[Growable[StringExpr]]] { [b] => (_, _) ?=> (field: generic.Field[b]) =>
              val valueExpr: Option[StringExpr] =
                (field.annotations.optionalOfValue[Show.annotation.obfuscate], field.annotations.optionalOfValue[Show.annotation.hide]) match {
                  case (None, None) =>
                    StringExpr.StrBuilder { builder => '{ ${ field.getExpr(instances) }.writeTo($builder, ${ field.fromParent(value) }) } }.some
                  case (Some(Show.annotation.obfuscate.map(char)), None) =>
                    StringExpr.StrBuilder { builder => '{ $builder.append(${ field.getExpr(instances) }.show(${ field.fromParent(value) }).map { _ => ${ Expr(char) } }) } }.some
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

        override def deriveCaseClass(generic: ProductGeneric.CaseClassGeneric[A]): Expr[Show[A]] =
          '{
            new Show.PreferBuffer[A] {
              override def writeTo(builder: mutable.StringBuilder, value: A): Unit = ${ makeWriteTo('builder, 'value) }
            }
          }

        override def deriveAnyVal[B: Type](generic: ProductGeneric.AnyValGeneric[A, B]): Expr[Show[A]] =
          '{
            new Show[A] {
              override def show(value: A): String =
                ${ generic.field.getExpr(instances) }.show(${ generic.singleField.unwrap('value) })
              override def writeTo(builder: mutable.StringBuilder, value: A): Unit =
                ${ generic.field.getExpr(instances) }.writeTo(builder, ${ generic.singleField.unwrap('value) })
            }
          }

        override def deriveCaseObject(generic: ProductGeneric.CaseObjectGeneric[A]): Expr[Show[A]] =
          '{
            new Show[A] {
              override def show(value: A): String = ${ Expr(generic.annotations.optionalOfValue[annotation.typeName].fold(generic.label)(_.name)) }
            }
          }

      }
    }

  override protected def sumDeriver[A](using Quotes, Type[Show], Type[A], SumGeneric[A], Derivable[Show]): Derivable.SumDeriver[Show, A] =
    Derivable.SumDeriver.withInstances[Show, A] { instances =>
      new Derivable.SumDeriver[Show, A] {

        private def showChild(builder: Expr[mutable.StringBuilder], value: Expr[A]): Expr[Unit] =
          generic.matcher.instance[Unit](value) { [b <: A] => (_, _) ?=> (kase: generic.Case[b]) =>
            kase.caseExtractor.withRHS { bValue =>
              '{ ${ kase.getExpr(instances) }.writeTo($builder, $bValue) }
            }
          }

        override def derive: Expr[Show[A]] =
          '{
            new Show.PreferBuffer[A] {
              override def writeTo(builder: mutable.StringBuilder, value: A): Unit = {
                builder.append(${ Expr(generic.annotations.optionalOfValue[annotation.typeName].fold(generic.label)(_.name) + ".") })
                ${ showChild('builder, 'value) }
              }
            }
          }

      }
    }

  inline def derived[A]: Show[A] = ${ derivedImpl[A] }

}

object ShowLowPriority {

  trait LowPriority1 {

    given seq: [S[_]: SeqRead as seqOps, A: Show as showA] => Show[S[A]] =
      Show.showSeq[S, A]("[", ", ", "]")

    given map: [K: Show as showKey, V: Show as showValue] => Show[Map[K, V]] =
      _.iterator.map { (k, v) => s" ${showKey.show(k)} : ${showValue.show(v)} " }.mkString("{", ",", "}")

  }

}
