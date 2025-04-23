package oxygen.json

import java.time.*
import java.util.TimeZone
import oxygen.core.javaEnums.given
import oxygen.core.typeclass.{NonEmpty, SeqOps}
import oxygen.json.generic.*
import oxygen.meta.*
import oxygen.predef.core.*
import scala.quoted.*

trait JsonEncoder[A] {

  def encodeJsonAST(value: A): Json

  def addToObject(value: A): Boolean = true

  final def encodeJsonStringCompact(value: A): String =
    encodeJsonAST(value).showCompact

  final def encodeJsonStringPretty(value: A): String =
    encodeJsonAST(value).showPretty

  final def contramap[B](f: B => A): JsonEncoder[B] =
    JsonEncoder.Contramapped(this, f)

  final def mapJsonOutput(f: PartialFunction[Json, Json]): JsonEncoder[A] =
    JsonEncoder.MapJsonOutput(this, json => f.lift(json).getOrElse(json))

}
object JsonEncoder extends K0.Derivable.WithInstances[JsonEncoder], JsonEncoderLowPriority.LowPriority1 {

  inline def apply[A](using ev: JsonEncoder[A]): JsonEncoder[A] = ev

  //////////////////////////////////////////////////////////////////////////////////////////////////////
  //      Instances
  //////////////////////////////////////////////////////////////////////////////////////////////////////

  final class AnyJsonEncoder[A <: Json] extends JsonEncoder[A] {
    override def encodeJsonAST(value: A): Json =
      value
  }

  object BigDecimalEncoder extends JsonEncoder[BigDecimal] {
    override def encodeJsonAST(value: BigDecimal): Json =
      Json.number(value)
  }

  object StringEncoder extends JsonEncoder[String] {
    override def encodeJsonAST(value: String): Json =
      Json.string(value)
  }

  object BooleanEncoder extends JsonEncoder[Boolean] {
    override def encodeJsonAST(value: Boolean): Json =
      Json.boolean(value)
  }

  final case class OptionEncoder[A](encoder: JsonEncoder[A]) extends JsonEncoder[Option[A]] {
    override def encodeJsonAST(value: Option[A]): Json = value match
      case Some(value) => encoder.encodeJsonAST(value)
      case None        => Json.Null
    override def addToObject(value: Option[A]): Boolean =
      value.nonEmpty
  }

  final case class SpecifiedEncoder[A](encoder: JsonEncoder[A]) extends JsonEncoder[Specified[A]] {
    override def encodeJsonAST(value: Specified[A]): Json = value match
      case Specified.WasSpecified(value) => encoder.encodeJsonAST(value)
      case Specified.WasNotSpecified     => Json.Null
    override def addToObject(value: Specified[A]): Boolean =
      value.toOption.nonEmpty
  }

  final case class ContiguousEncoder[A](encoder: JsonEncoder[A]) extends JsonEncoder[Contiguous[A]] {
    override def encodeJsonAST(value: Contiguous[A]): Json =
      Json.Arr(value.map(encoder.encodeJsonAST))
  }

  final case class MapEncoder[K, V](k: JsonFieldEncoder[K], v: JsonEncoder[V]) extends JsonEncoder[Map[K, V]] {
    override def encodeJsonAST(value: Map[K, V]): Json =
      Json.Obj(Contiguous.from(value).map { case (_k, _v) => (k.encode(_k), v.encodeJsonAST(_v)) })
  }

  sealed trait TupleEncoder[A <: Tuple] extends JsonEncoder[A] {

    val size: Int

    private[TupleEncoder] def append(value: A, offset: Int, array: Array[Json]): Unit

    override final def encodeJsonAST(value: A): Json = {
      val (contiguous, array) = Contiguous.unsafeMakeExposingArray[Json](size)
      append(value, 0, array)
      Json.Arr(contiguous)
    }

  }
  object TupleEncoder {

    final case class Append[A, B <: Tuple](a: JsonEncoder[A], b: TupleEncoder[B]) extends TupleEncoder[A *: B] {

      override val size: Int = b.size + 1

      override private[TupleEncoder] def append(value: A *: B, offset: Int, array: Array[Json]): Unit =
        value match {
          case aValue *: bValue =>
            array(offset) = a.encodeJsonAST(aValue)
            b.append(bValue, offset + 1, array)
        }

    }

    case object Empty extends TupleEncoder[EmptyTuple] {

      override val size: Int = 0

      override private[TupleEncoder] def append(value: EmptyTuple, offset: Int, array: Array[Json]): Unit =
        ()

    }

    given emptyTuple: TupleEncoder[EmptyTuple] = TupleEncoder.Empty
    given tupleAppend: [A, B <: Tuple] => (a: JsonEncoder[A], b: TupleEncoder[B]) => TupleEncoder[A *: B] = TupleEncoder.Append(a, b)

  }

  //////////////////////////////////////////////////////////////////////////////////////////////////////
  //      Givens
  //////////////////////////////////////////////////////////////////////////////////////////////////////

  def usingToString[A]: JsonEncoder[A] = StringEncoder.contramap(_.toString)

  given json: [A <: Json] => JsonEncoder[A] = new AnyJsonEncoder[A]

  given string: JsonEncoder[String] = StringEncoder
  given boolean: JsonEncoder[Boolean] = BooleanEncoder

  given bigDecimal: JsonEncoder[BigDecimal] = BigDecimalEncoder
  given double: JsonEncoder[Double] = BigDecimalEncoder.contramap(BigDecimal.exact)
  given float: JsonEncoder[Float] = BigDecimalEncoder.contramap(BigDecimal.exact)
  given bigInt: JsonEncoder[BigInt] = BigDecimalEncoder.contramap(BigDecimal.exact)
  given long: JsonEncoder[Long] = BigDecimalEncoder.contramap(BigDecimal.exact)
  given int: JsonEncoder[Int] = BigDecimalEncoder.contramap(BigDecimal.exact)
  given short: JsonEncoder[Short] = BigDecimalEncoder.contramap(BigDecimal.exact)
  given byte: JsonEncoder[Byte] = BigDecimalEncoder.contramap(BigDecimal.exact)

  given option: [A] => (encoder: JsonEncoder[A]) => JsonEncoder[Option[A]] = OptionEncoder(encoder)
  given specified: [A] => (encoder: JsonEncoder[A]) => JsonEncoder[Specified[A]] = SpecifiedEncoder(encoder)

  given contiguous: [A] => (encoder: JsonEncoder[A]) => JsonEncoder[Contiguous[A]] = ContiguousEncoder(encoder)
  given seq: [S[_], A] => (seqOps: SeqOps[S], encoder: JsonEncoder[A]) => JsonEncoder[S[A]] = ContiguousEncoder(encoder).contramap(_.transformTo[Contiguous])

  given map: [K, V] => (k: JsonFieldEncoder[K], v: JsonEncoder[V]) => JsonEncoder[Map[K, V]] = MapEncoder(k, v)

  given tuple: [A <: Tuple] => (enc: TupleEncoder[A]) => JsonEncoder[A] = enc

  given `enum`: [A] => (ec: Enum.Companion[A]) => JsonEncoder[A] = StringEncoder.contramap(ec.ToString.encode)

  given localTime: JsonEncoder[LocalTime] = usingToString
  given localDate: JsonEncoder[LocalDate] = usingToString
  given localDateTime: JsonEncoder[LocalDateTime] = usingToString
  given zonedDateTime: JsonEncoder[ZonedDateTime] = usingToString
  given offsetDateTime: JsonEncoder[OffsetDateTime] = usingToString
  given offsetTime: JsonEncoder[OffsetTime] = usingToString
  given instant: JsonEncoder[Instant] = usingToString
  given duration: JsonEncoder[Duration] = usingToString
  given period: JsonEncoder[Period] = usingToString
  given zoneId: JsonEncoder[ZoneId] = usingToString
  given zoneOffset: JsonEncoder[ZoneOffset] = usingToString
  given timeZone: JsonEncoder[TimeZone] = usingToString
  given monthDay: JsonEncoder[MonthDay] = usingToString
  given year: JsonEncoder[Year] = usingToString
  given yearMonth: JsonEncoder[YearMonth] = usingToString
  given month: JsonEncoder[Month] = `enum`[Month]

  //////////////////////////////////////////////////////////////////////////////////////////////////////
  //      Transformers
  //////////////////////////////////////////////////////////////////////////////////////////////////////

  final case class Contramapped[A, B](encoder: JsonEncoder[A], f: B => A) extends JsonEncoder[B] {
    override def encodeJsonAST(value: B): Json =
      encoder.encodeJsonAST(f(value))
    override def addToObject(value: B): Boolean =
      encoder.addToObject(f(value))
  }

  final case class MapJsonOutput[A](encoder: JsonEncoder[A], f: Json => Json) extends JsonEncoder[A] {
    override def encodeJsonAST(value: A): Json =
      f(encoder.encodeJsonAST(value))
    override def addToObject(value: A): Boolean =
      encoder.addToObject(value)
  }

  //////////////////////////////////////////////////////////////////////////////////////////////////////
  //      Generic
  //////////////////////////////////////////////////////////////////////////////////////////////////////

  override protected def internalDeriveProductI[Q <: Quotes, A](k0: K0[Q])(
      g: k0.ProductGeneric[A],
      i: k0.ValExpressions[JsonEncoder],
  )(using quotes: Q, aTpe: Type[A], tTpe: Type[JsonEncoder]): Expr[JsonEncoder[A]] =
    DeriveProductJsonEncoder[Q, A](k0)(g, i).makeJsonEncoder

  override protected def internalDeriveSumI[Q <: Quotes, A](k0: K0[Q])(
      g: k0.SumGeneric[A],
      i: k0.ValExpressions[JsonEncoder],
  )(using quotes: Q, aTpe: Type[A], tTpe: Type[JsonEncoder]): Expr[JsonEncoder[A]] =
    DeriveSumJsonEncoder(k0)(g, i).makeJsonEncoder

  inline def derived[A]: JsonEncoder[A] = ${ derivedImpl[A] }

}

object JsonEncoderLowPriority {

  trait LowPriority1 {

    given fromJsonCodec: [A] => (codec: JsonCodec[A]) => JsonEncoder[A] = codec.encoder

    given nonEmptySeq: [S1[_], S2[_], A] => (ne: NonEmpty.Aux[S1, S2]) => (encoder: JsonEncoder[S1[A]]) => JsonEncoder[S2[A]] =
      encoder.contramap(ne.eraseNonEmpty)

  }

}
