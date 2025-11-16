package oxygen.json

import java.time.*
import java.util.{TimeZone, UUID}
import oxygen.core.javaEnums.given
import oxygen.core.typeclass.{NonEmpty, SeqOps}
import oxygen.json.generic.*
import oxygen.meta.*
import oxygen.predef.core.*
import scala.quoted.*
import scala.util.Try

trait JsonDecoder[A] {

  def decodeJsonAST(ast: Json): Either[JsonError, A]

  val onMissingFromObject: Option[A] = None

  // TODO (KR) : implement in-line parser
  final def decodeJsonString(string: String): Either[JsonError, A] =
    Json.parse(string).flatMap(decodeJsonAST)

  def map[B](f: A => B): JsonDecoder[B] =
    JsonDecoder.Mapped(this, f)

  def mapOrFail[B](f: A => Either[String, B]): JsonDecoder[B] =
    JsonDecoder.MappedOrFail(this, f)

  final def mapAttempt[B](f: A => B)(using typeTag: TypeTag[B]): JsonDecoder[B] =
    mapOrFail { a => Try { f(a) }.toOption.toRight(s"Invalid ${typeTag.prefixNone}: $a") }

  final def mapJsonInput(f: PartialFunction[Json, Json]): JsonDecoder[A] =
    JsonDecoder.MapJsonInput(this, json => f.lift(json).getOrElse(json))

  final def <>[B >: A](that: JsonDecoder[B]): JsonDecoder[B] =
    JsonDecoder.OrElse(this, that)

  inline final def autoMap[B]: JsonDecoder[B] = {
    val (ab, _) = K0.ProductGeneric.deriveTransform[A, B]
    map(ab)
  }

}
object JsonDecoder extends K0.Derivable[JsonDecoder.ObjectDecoder], JsonDecoderLowPriority.LowPriority1 {

  inline def apply[A](using ev: JsonDecoder[A]): JsonDecoder[A] = ev

  //////////////////////////////////////////////////////////////////////////////////////////////////////
  //      Instances
  //////////////////////////////////////////////////////////////////////////////////////////////////////

  trait ObjectDecoder[A] extends JsonDecoder[A] {

    def decodeJsonObjectAST(ast: Json.Obj): Either[JsonError, A]

    override def map[B](f: A => B): JsonDecoder.ObjectDecoder[B] =
      JsonDecoder.MappedObject(this, f)

    override def mapOrFail[B](f: A => Either[String, B]): JsonDecoder.ObjectDecoder[B] =
      JsonDecoder.MappedOrFailObject(this, f)

    override final def decodeJsonAST(ast: Json): Either[JsonError, A] = ast match
      case ast: Json.Obj => decodeJsonObjectAST(ast)
      case _             => JsonError(Nil, JsonError.Cause.InvalidType(Json.Type.Object, ast.tpe)).asLeft

  }

  object StringDecoder extends JsonDecoder[String] {
    override def decodeJsonAST(ast: Json): Either[JsonError, String] = ast match
      case Json.Str(value) => value.asRight
      case _               => JsonError(Nil, JsonError.Cause.InvalidType(Json.Type.String, ast.tpe)).asLeft
  }

  object BooleanDecoder extends JsonDecoder[Boolean] {
    override def decodeJsonAST(ast: Json): Either[JsonError, Boolean] = ast match
      case Json.Bool(value) => value.asRight
      case _                => JsonError(Nil, JsonError.Cause.InvalidType(Json.Type.Boolean, ast.tpe)).asLeft
  }

  object AnyJsonDecoder extends JsonDecoder[Json] {
    override def decodeJsonAST(ast: Json): Either[JsonError, Json] =
      ast.asRight
  }

  final case class JsonSubtypeDecoder[A <: Json](filter: Json => Option[A], expTpe: Json.Type) extends JsonDecoder[A] {
    override def decodeJsonAST(ast: Json): Either[JsonError, A] =
      filter(ast).toRight(JsonError(Nil, JsonError.Cause.InvalidType(expTpe, ast.tpe)))
  }

  object BigDecimalDecoder extends JsonDecoder[BigDecimal] {

    private object stringToBigDecimal {
      def unapply(string: String): Option[BigDecimal] =
        Try { BigDecimal(string) }.toOption
    }

    override def decodeJsonAST(ast: Json): Either[JsonError, BigDecimal] = ast match
      case Json.Number(value)                  => value.asRight
      case Json.Str(stringToBigDecimal(value)) => value.asRight
      case _                                   => JsonError(Nil, JsonError.Cause.InvalidType(Json.Type.Number, ast.tpe)).asLeft

    def narrow[A](to: BigDecimal => A, from: A => BigDecimal): JsonDecoder[A] =
      mapOrFail { bigDecimal =>
        try {
          val converted = to(bigDecimal)
          if (from(converted) == bigDecimal) converted.asRight
          else "Numeric overflow".asLeft
        } catch {
          case e: ArithmeticException => e.safeGetMessage.asLeft
        }
      }

    def narrowWhole[A](to: BigDecimal => A, from: A => BigDecimal): JsonDecoder[A] =
      mapOrFail { bigDecimal =>
        if (bigDecimal.isWhole)
          try {
            val converted = to(bigDecimal)
            if (from(converted) == bigDecimal) converted.asRight
            else "Numeric overflow".asLeft
          } catch {
            case e: ArithmeticException => e.safeGetMessage.asLeft
          }
        else
          "Non decimal expected".asLeft
      }

  }

  final case class OptionDecoder[A](decoder: JsonDecoder[A]) extends JsonDecoder[Option[A]] {
    override def decodeJsonAST(ast: Json): Either[JsonError, Option[A]] = ast match
      case Json.Null => None.asRight
      case _         => decoder.decodeJsonAST(ast).map(_.some)
    override val onMissingFromObject: Option[Option[A]] =
      None.some
  }

  final case class SpecifiedDecoder[A](decoder: JsonDecoder[A]) extends JsonDecoder[Specified[A]] {
    override def decodeJsonAST(ast: Json): Either[JsonError, Specified[A]] =
      decoder.decodeJsonAST(ast).map(Specified.WasSpecified(_))
    override val onMissingFromObject: Option[Specified[A]] =
      Specified.WasNotSpecified.some
  }

  final case class ArraySeqDecoder[A](decoder: JsonDecoder[A]) extends JsonDecoder[ArraySeq[A]] {
    override def decodeJsonAST(ast: Json): Either[JsonError, ArraySeq[A]] = ast match
      case Json.Arr(value) => value.traverse(decoder.decodeJsonAST)
      case _               => JsonError(Nil, JsonError.Cause.InvalidType(Json.Type.Array, ast.tpe)).asLeft
  }

  final case class MapDecoder[K, V](k: JsonFieldDecoder[K], v: JsonDecoder[V]) extends JsonDecoder[Map[K, V]] {
    override def decodeJsonAST(ast: Json): Either[JsonError, Map[K, V]] = ast match {
      case Json.Obj(value) =>
        value
          .traverse { case (_k, _v) =>
            for {
              k2 <- k.decode(_k).leftMap(e => JsonError(JsonError.Path.Field(_k) :: Nil, JsonError.Cause.InvalidKey(e)))
              v2 <- v.decodeJsonAST(_v).leftMap(_.inField(_k))
            } yield (k2, v2)
          }
          .map(_.toMap)
      case _ => JsonError(Nil, JsonError.Cause.InvalidType(Json.Type.Object, ast.tpe)).asLeft
    }
  }

  sealed trait TupleDecoder[A <: Tuple] extends JsonDecoder[A] {

    val size: Int

    private[TupleDecoder] def decodeArray(in: ArraySeq[Json], offset: Int): Either[JsonError, A]

    override final def decodeJsonAST(ast: Json): Either[JsonError, A] = ast match
      case Json.Arr(value) if value.length == size => decodeArray(value, 0)
      case Json.Arr(value)                         => JsonError(Nil, JsonError.Cause.DecodingFailed(s"Expected array of size $size, but got size ${value.length}")).asLeft
      case _                                       => JsonError(Nil, JsonError.Cause.InvalidType(Json.Type.Array, ast.tpe)).asLeft

  }
  object TupleDecoder {

    final case class Append[A, B <: Tuple](a: JsonDecoder[A], b: TupleDecoder[B]) extends TupleDecoder[A *: B] {

      override val size: Int = b.size + 1

      override private[TupleDecoder] def decodeArray(in: ArraySeq[Json], offset: Int): Either[JsonError, A *: B] =
        for {
          aValue <- a.decodeJsonAST(in(offset)).leftMap(_.atIndex(offset))
          bValue <- b.decodeArray(in, offset + 1)
        } yield aValue *: bValue

    }

    case object Empty extends TupleDecoder[EmptyTuple] {

      override val size: Int = 0

      override private[TupleDecoder] def decodeArray(in: ArraySeq[Json], offset: Int): Either[JsonError, EmptyTuple] =
        EmptyTuple.asRight

    }

    given emptyTuple: TupleDecoder[EmptyTuple] = TupleDecoder.Empty
    given tupleAppend: [A, B <: Tuple] => (a: JsonDecoder[A], b: TupleDecoder[B]) => TupleDecoder[A *: B] = TupleDecoder.Append(a, b)

  }

  final case class OrElse[A, B >: A](a: JsonDecoder[A], b: JsonDecoder[B]) extends JsonDecoder[B] {

    override def decodeJsonAST(ast: Json): Either[JsonError, B] =
      a.decodeJsonAST(ast) match {
        case Right(value) => value.asRight
        case Left(_)      => b.decodeJsonAST(ast)
      }

  }

  //////////////////////////////////////////////////////////////////////////////////////////////////////
  //      Givens
  //////////////////////////////////////////////////////////////////////////////////////////////////////

  given json: JsonDecoder[Json] = AnyJsonDecoder
  given jsonString: JsonDecoder[Json.Str] = JsonSubtypeDecoder(_.toJsonString, Json.Type.String)
  given jsonBoolean: JsonDecoder[Json.Bool] = JsonSubtypeDecoder(_.toJsonBoolean, Json.Type.Boolean)
  given jsonNumber: JsonDecoder[Json.Number] = JsonSubtypeDecoder(_.toJsonNumber, Json.Type.Number)
  given jsonArray: JsonDecoder[Json.Arr] = JsonSubtypeDecoder(_.toJsonArray, Json.Type.Array)
  given jsonObject: JsonDecoder[Json.Obj] = JsonSubtypeDecoder(_.toJsonObject, Json.Type.Object)
  given jsonNull: JsonDecoder[Json.Null.type] = JsonSubtypeDecoder(_.toJsonNull, Json.Type.Null)

  given string: JsonDecoder[String] = StringDecoder
  given boolean: JsonDecoder[Boolean] = BooleanDecoder
  given uuid: JsonDecoder[UUID] = StringDecoder.mapAttempt(UUID.fromString)

  given bigDecimal: JsonDecoder[BigDecimal] = BigDecimalDecoder
  given double: JsonDecoder[Double] = BigDecimalDecoder.narrow(_.toDouble, BigDecimal.exact)
  given float: JsonDecoder[Float] = BigDecimalDecoder.narrow(_.toFloat, BigDecimal.exact)
  given bigInt: JsonDecoder[BigInt] = BigDecimalDecoder.narrow(_.toBigInt, BigDecimal.exact)
  given long: JsonDecoder[Long] = BigDecimalDecoder.narrow(_.toLong, BigDecimal.exact)
  given int: JsonDecoder[Int] = BigDecimalDecoder.narrow(_.toInt, BigDecimal.exact)
  given short: JsonDecoder[Short] = BigDecimalDecoder.narrow(_.toShort, BigDecimal.exact)
  given byte: JsonDecoder[Byte] = BigDecimalDecoder.narrow(_.toByte, BigDecimal.exact)

  given option: [A] => (decoder: JsonDecoder[A]) => JsonDecoder[Option[A]] = OptionDecoder(decoder)
  given specified: [A] => (decoder: JsonDecoder[A]) => JsonDecoder[Specified[A]] = SpecifiedDecoder(decoder)

  given arraySeq: [A] => (decoder: JsonDecoder[A]) => JsonDecoder[ArraySeq[A]] = ArraySeqDecoder(decoder)
  given seq: [S[_], A] => (seqOps: SeqOps[S], decoder: JsonDecoder[A]) => JsonDecoder[S[A]] = ArraySeqDecoder(decoder).map(_.transformTo[S])

  given map: [K, V] => (k: JsonFieldDecoder[K], v: JsonDecoder[V]) => JsonDecoder[Map[K, V]] = MapDecoder(k, v)

  given tuple: [A <: Tuple] => (dec: TupleDecoder[A]) => JsonDecoder[A] = dec

  given strictEnum: [A: StrictEnum as e] => JsonDecoder[A] =
    StringDecoder.mapOrFail(e.decodeEitherWithHint)

  given enumWithOther: [A: EnumWithOther as e] => JsonDecoder[A] =
    StringDecoder.map(e.decode)

  def `enum`[A: StrictEnum]: JsonDecoder[A] = strictEnum[A]

  given localTime: JsonDecoder[LocalTime] = StringDecoder.mapAttempt(LocalTime.parse)
  given localDate: JsonDecoder[LocalDate] = StringDecoder.mapAttempt(LocalDate.parse)
  given localDateTime: JsonDecoder[LocalDateTime] = StringDecoder.mapAttempt(LocalDateTime.parse)
  given zonedDateTime: JsonDecoder[ZonedDateTime] = StringDecoder.mapAttempt(ZonedDateTime.parse)
  given offsetDateTime: JsonDecoder[OffsetDateTime] = StringDecoder.mapAttempt(OffsetDateTime.parse)
  given offsetTime: JsonDecoder[OffsetTime] = StringDecoder.mapAttempt(OffsetTime.parse)
  given instant: JsonDecoder[Instant] = StringDecoder.mapAttempt(Instant.parse)
  given duration: JsonDecoder[Duration] = StringDecoder.mapAttempt(Duration.parse)
  given period: JsonDecoder[Period] = StringDecoder.mapAttempt(Period.parse)
  given zoneId: JsonDecoder[ZoneId] = StringDecoder.mapAttempt(ZoneId.of)
  given zoneOffset: JsonDecoder[ZoneOffset] = StringDecoder.mapAttempt(ZoneOffset.of)
  given timeZone: JsonDecoder[TimeZone] = StringDecoder.mapAttempt(TimeZone.getTimeZone)
  given monthDay: JsonDecoder[MonthDay] = StringDecoder.mapAttempt(MonthDay.parse)
  given year: JsonDecoder[Year] = StringDecoder.mapAttempt(Year.parse)
  given yearMonth: JsonDecoder[YearMonth] = StringDecoder.mapAttempt(YearMonth.parse)
  given month: JsonDecoder[Month] = `enum`[Month]

  //////////////////////////////////////////////////////////////////////////////////////////////////////
  //      Transformers
  //////////////////////////////////////////////////////////////////////////////////////////////////////

  final case class Mapped[A, B](decoder: JsonDecoder[A], f: A => B) extends JsonDecoder[B] {
    override def decodeJsonAST(ast: Json): Either[JsonError, B] =
      decoder.decodeJsonAST(ast).map(f)
    override val onMissingFromObject: Option[B] =
      decoder.onMissingFromObject.map(f)
  }

  final case class MappedOrFail[A, B](decoder: JsonDecoder[A], f: A => Either[String, B]) extends JsonDecoder[B] {
    override def decodeJsonAST(ast: Json): Either[JsonError, B] =
      decoder.decodeJsonAST(ast).flatMap(f(_).leftMap(e => JsonError(Nil, JsonError.Cause.DecodingFailed(e))))
    override val onMissingFromObject: Option[B] =
      decoder.onMissingFromObject.flatMap(f(_).toOption)
  }

  final case class MappedObject[A, B](decoder: JsonDecoder.ObjectDecoder[A], f: A => B) extends JsonDecoder.ObjectDecoder[B] {
    override def decodeJsonObjectAST(ast: Json.Obj): Either[JsonError, B] =
      decoder.decodeJsonObjectAST(ast).map(f)
    override val onMissingFromObject: Option[B] =
      decoder.onMissingFromObject.map(f)
  }

  final case class MappedOrFailObject[A, B](decoder: JsonDecoder.ObjectDecoder[A], f: A => Either[String, B]) extends JsonDecoder.ObjectDecoder[B] {
    override def decodeJsonObjectAST(ast: Json.Obj): Either[JsonError, B] =
      decoder.decodeJsonObjectAST(ast).flatMap(f(_).leftMap(e => JsonError(Nil, JsonError.Cause.DecodingFailed(e))))
    override val onMissingFromObject: Option[B] =
      decoder.onMissingFromObject.flatMap(f(_).toOption)
  }

  final case class MapJsonInput[A](decoder: JsonDecoder[A], f: Json => Json) extends JsonDecoder[A] {
    override def decodeJsonAST(ast: Json): Either[JsonError, A] =
      decoder.decodeJsonAST(f(ast))
    override val onMissingFromObject: Option[A] =
      decoder.onMissingFromObject
  }

  //////////////////////////////////////////////////////////////////////////////////////////////////////
  //      Generic
  //////////////////////////////////////////////////////////////////////////////////////////////////////

  override protected def productDeriver[A](using
      Quotes,
      Type[JsonDecoder.ObjectDecoder],
      Type[A],
      K0.ProductGeneric[A],
      K0.Derivable[JsonDecoder.ObjectDecoder],
  ): K0.Derivable.ProductDeriver[JsonDecoder.ObjectDecoder, A] =
    K0.Derivable.ProductDeriver.withDisjointInstances[JsonDecoder.ObjectDecoder, JsonDecoder, A] { DeriveProductJsonDecoder[A](_) }

  override protected def sumDeriver[A](using
      Quotes,
      Type[JsonDecoder.ObjectDecoder],
      Type[A],
      K0.SumGeneric[A],
      K0.Derivable[JsonDecoder.ObjectDecoder],
  ): K0.Derivable.SumDeriver[JsonDecoder.ObjectDecoder, A] =
    K0.Derivable.SumDeriver.withInstances { DeriveSumJsonDecoder[A](_) }

  override inline def derived[A]: JsonDecoder.ObjectDecoder[A] = ${ derivedImpl[A] }

  private def deriveWrappedImpl[A: Type](using Quotes): Expr[JsonDecoder[A]] = {
    type B
    val wrapping = K0.ProductGeneric.extractSingleCaseClassField[A, B]
    given Type[B] = wrapping.field.tpe

    '{ ${ wrapping.field.summonTypeClass[JsonDecoder] }.map[A](${ wrapping.wrapExpr }) }
  }

  inline def deriveWrapped[A]: JsonDecoder[A] = ${ deriveWrappedImpl[A] }

}

object JsonDecoderLowPriority {

  trait LowPriority1 {

    given fromJsonCodec: [A] => (codec: JsonCodec[A]) => JsonDecoder[A] = codec.decoder

    given nonEmptySeq: [S1[_], S2[_], A] => (ne: NonEmpty.Aux[S1, S2]) => (decoder: JsonDecoder[S1[A]]) => JsonDecoder[S2[A]] =
      decoder.mapOrFail(ne.nonEmpty(_).toRight("Expected non-empty Array"))

  }

}
