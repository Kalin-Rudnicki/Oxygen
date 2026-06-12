package oxygen.json

import java.time.*
import java.util.{TimeZone, UUID}
import oxygen.core.javaEnums.given
import oxygen.core.model.Email
import oxygen.core.typeclass.{NonEmpty, SeqOps}
import oxygen.json.generic.*
import oxygen.meta.k0.*
import oxygen.predef.core.*
import oxygen.quoted.ValDef
import scala.quoted.*
import scala.reflect.ClassTag

trait JsonEncoder[A] {

  def encodeJsonAST(value: A): Json
  def encodeSplitJsonAST(value: A): Ior[PlainTextJson, SecretJson]

  final def encodeSplitRoot(value: A): (PlainTextJson, SecretJson) = encodeSplitJsonAST(value) match
    case Ior.Both(left, right) => (left, right)
    case Ior.Left(left)        => (left, SecretJsonObject(ArraySeq.empty))
    case Ior.Right(right)      => (PlainTextJsonObject(ArraySeq.empty), right)

  final def encodeSplitRootCompact(value: A): (PlainTextJsonFormattedString, SecretJsonFormattedString) = {
    val (plainText, secret) = encodeSplitRoot(value)
    (PlainTextJsonFormattedString.wrap(plainText.showCompact), SecretJsonFormattedString.wrap(secret.showCompact))
  }

  final def encodeSplitRootPretty(value: A): (PlainTextJsonFormattedString, SecretJsonFormattedString) = {
    val (plainText, secret) = encodeSplitRoot(value)
    (PlainTextJsonFormattedString.wrap(plainText.showPretty), SecretJsonFormattedString.wrap(secret.showPretty))
  }

  def addToObject(value: A): Boolean = true

  final def encodeJsonStringCompact(value: A): String =
    encodeJsonAST(value).showCompact

  final def encodeJsonStringPretty(value: A): String =
    encodeJsonAST(value).showPretty

  def contramap[B](f: B => A): JsonEncoder[B] =
    JsonEncoder.Contramapped(this, f)

  final def mapJsonOutput(f: PartialFunction[Json, Json]): JsonEncoder[A] =
    JsonEncoder.MapJsonOutput(this, json => f.lift(json).getOrElse(json))

  inline final def autoContramap[B]: JsonEncoder[B] = {
    val (_, ba) = ProductGeneric.deriveTransform[A, B]
    contramap(ba)
  }

  final def toStringEncoderCompact: StringEncoder[A] = StringEncoder.string.contramap(this.encodeJsonStringCompact)
  final def toStringEncoderPretty: StringEncoder[A] = StringEncoder.string.contramap(this.encodeJsonStringPretty)
  final def toStringEncoder: StringEncoder[A] = toStringEncoderCompact

  final def toObjectEncoderOrThrow: JsonEncoder.ObjectEncoder[A] = this match
    case self: JsonEncoder.ObjectEncoder[A] => self
    case _                                  => throw new RuntimeException(s"Not a `JsonEncoder.ObjectEncoder`: ${this.getClass.getName}\n$this")

  def secret: JsonEncoder.Secret[A] = JsonEncoder.Secret.fromJsonEncoder(this)

}
object JsonEncoder extends Derivable[JsonEncoder.ObjectEncoder], JsonEncoderLowPriority.LowPriority1 {

  inline def apply[A](using ev: JsonEncoder[A]): JsonEncoder[A] = ev

  //////////////////////////////////////////////////////////////////////////////////////////////////////
  //      Givens
  //////////////////////////////////////////////////////////////////////////////////////////////////////

  def usingToString[A]: JsonEncoder[A] = StringEncoder.contramap(_.toString)

  given json: [A <: Json] => JsonEncoder[A] = new AnyJsonEncoder[A]

  given string: JsonEncoder[String] = StringEncoder
  given text: JsonEncoder[Text] = usingToString
  given boolean: JsonEncoder[Boolean] = BooleanEncoder
  given uuid: JsonEncoder[UUID] = usingToString

  given bigDecimal: JsonEncoder[BigDecimal] = BigDecimalEncoder
  given double: JsonEncoder[Double] = BigDecimalEncoder.contramap(BigDecimal.exact)
  given float: JsonEncoder[Float] = BigDecimalEncoder.contramap(BigDecimal.exact)
  given bigInt: JsonEncoder[BigInt] = BigDecimalEncoder.contramap(BigDecimal.exact)
  given long: JsonEncoder[Long] = BigDecimalEncoder.contramap(BigDecimal.exact)
  given int: JsonEncoder[Int] = BigDecimalEncoder.contramap(BigDecimal.exact)
  given short: JsonEncoder[Short] = BigDecimalEncoder.contramap(BigDecimal.exact)
  given byte: JsonEncoder[Byte] = BigDecimalEncoder.contramap(BigDecimal.exact)

  given option: [A] => (encoder: JsonEncoder[A]) => JsonEncoder[Option[A]] = OptionEncoder(encoder)
  given nullable: [A] => (encoder: JsonEncoder[A]) => JsonEncoder[Nullable[A]] = NullableEncoder(encoder)
  given specified: [A] => (encoder: JsonEncoder[A]) => JsonEncoder[Specified[A]] = SpecifiedEncoder(encoder)

  given arraySeq: [A] => (encoder: JsonEncoder[A]) => JsonEncoder[ArraySeq[A]] = ArraySeqEncoder(encoder)
  given seq: [S[_], A] => (seqOps: SeqOps[S], encoder: JsonEncoder[A], ct: ClassTag[A]) => JsonEncoder[S[A]] = ArraySeqEncoder(encoder).contramap(_.toArraySeq)

  given map: [K, V] => (k: JsonFieldEncoder[K], v: JsonEncoder[V]) => JsonEncoder[Map[K, V]] = MapEncoder(k, v)
  given orderedMap: [K, V] => (k: JsonFieldEncoder[K], v: JsonEncoder[V]) => JsonEncoder[OrderedMap[K, V]] = OrderedMapEncoder(k, v)

  given tuple: [A <: Tuple] => (enc: TupleEncoder[A]) => JsonEncoder[A] = enc

  given strictEnum: [A: StrictEnum as e] => JsonEncoder[A] = StringEncoder.contramap(e.encode)
  given enumWithOther: [A: EnumWithOther as e] => JsonEncoder[A] = StringEncoder.contramap(e.encode)

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

  given email: JsonEncoder[Email] = usingToString

  //////////////////////////////////////////////////////////////////////////////////////////////////////
  //      Builders
  //////////////////////////////////////////////////////////////////////////////////////////////////////

  def `enum`[A: StrictEnum]: JsonEncoder[A] = strictEnum[A]

  def jsonStringUsingStringEncoder[A: StringEncoder as enc]: JsonEncoder[A] = JsonEncoder.string.contramap(enc.encode)

  def jsonStringFromStringEncoder[A](enc: StringEncoder[A]): JsonEncoder[A] = JsonEncoder.string.contramap(enc.encode)

  //////////////////////////////////////////////////////////////////////////////////////////////////////
  //      Instances
  //////////////////////////////////////////////////////////////////////////////////////////////////////

  trait Plain[A] extends JsonEncoder[A] {
    override def encodeSplitJsonAST(value: A): Ior[PlainTextJson, SecretJson] = Ior.Left(PlainTextJson.wrap(encodeJsonAST(value)))

  }

  trait ObjectEncoder[A] extends JsonEncoder[A] {

    def encodeJsonObjectFields(value: A): Growable[(String, Json)]
    def encodeSplitJsonObjectFields(value: A): Growable[(String, Ior[PlainTextJson, SecretJson])]

    override final def encodeJsonAST(value: A): Json.Obj = Json.Obj(encodeJsonObjectFields(value).toArraySeq)
    override def encodeSplitJsonAST(value: A): Ior[PlainTextJsonObject, SecretJsonObject] = SecretUtil.splitObjectElems(encodeSplitJsonObjectFields(value).toArraySeq)

    override def contramap[B](f: B => A): JsonEncoder.ObjectEncoder[B] =
      JsonEncoder.ContramappedObject(this, f)

    override def secret: JsonEncoder.Secret.ObjectEncoder[A] = JsonEncoder.Secret.fromJsonObjectEncoder(this)

  }

  final class AnyJsonEncoder[A <: Json] extends JsonEncoder.Plain[A] {
    override def encodeJsonAST(value: A): Json =
      value
  }

  object BigDecimalEncoder extends JsonEncoder.Plain[BigDecimal] {
    override def encodeJsonAST(value: BigDecimal): Json =
      Json.number(value)
  }

  object StringEncoder extends JsonEncoder.Plain[String] {
    override def encodeJsonAST(value: String): Json =
      Json.string(value)
  }

  object BooleanEncoder extends JsonEncoder.Plain[Boolean] {
    override def encodeJsonAST(value: Boolean): Json =
      Json.boolean(value)
  }

  final case class OptionEncoder[A](encoder: JsonEncoder[A]) extends JsonEncoder[Option[A]] {
    override def encodeJsonAST(value: Option[A]): Json = value match
      case Some(value) => encoder.encodeJsonAST(value)
      case None        => Json.Null
    override def encodeSplitJsonAST(value: Option[A]): Ior[PlainTextJson, SecretJson] = value match
      case Some(value) => encoder.encodeSplitJsonAST(value)
      case None        => Ior.Left(PlainTextJson.Null)
    override def addToObject(value: Option[A]): Boolean =
      value.nonEmpty
  }

  final case class NullableEncoder[A](encoder: JsonEncoder[A]) extends JsonEncoder[Nullable[A]] {
    override def encodeJsonAST(value: Nullable[A]): Json = value.unwrap match
      case Some(value) => encoder.encodeJsonAST(value)
      case None        => Json.Null
    override def encodeSplitJsonAST(value: Nullable[A]): Ior[PlainTextJson, SecretJson] = value.unwrap match
      case Some(value) => encoder.encodeSplitJsonAST(value)
      case None        => Ior.Left(PlainTextJson.Null)
  }

  final case class SpecifiedEncoder[A](encoder: JsonEncoder[A]) extends JsonEncoder[Specified[A]] {
    override def encodeJsonAST(value: Specified[A]): Json = value match
      case Specified.WasSpecified(value) => encoder.encodeJsonAST(value)
      case Specified.WasNotSpecified     => Json.Null
    override def encodeSplitJsonAST(value: Specified[A]): Ior[PlainTextJson, SecretJson] = value match
      case Specified.WasSpecified(value) => encoder.encodeSplitJsonAST(value)
      case Specified.WasNotSpecified     => Ior.Left(PlainTextJson.Null)
    override def addToObject(value: Specified[A]): Boolean =
      value.toOption.nonEmpty
  }

  final case class ArraySeqEncoder[A](encoder: JsonEncoder[A]) extends JsonEncoder[ArraySeq[A]] {
    override def encodeJsonAST(value: ArraySeq[A]): Json =
      Json.Arr(value.map(encoder.encodeJsonAST))
    override def encodeSplitJsonAST(value: ArraySeq[A]): Ior[PlainTextJson, SecretJson] =
      SecretUtil.splitArrayElems(value.map(encoder.encodeSplitJsonAST))
  }

  final case class MapEncoder[K, V](k: JsonFieldEncoder[K], v: JsonEncoder[V]) extends JsonEncoder[Map[K, V]] {
    override def encodeJsonAST(value: Map[K, V]): Json =
      Json.Obj(ArraySeq.from(value).map { case (_k, _v) => (k.encode(_k), v.encodeJsonAST(_v)) })
    override def encodeSplitJsonAST(value: Map[K, V]): Ior[PlainTextJson, SecretJson] =
      SecretUtil.splitObjectElems(ArraySeq.from(value).map { case (_k, _v) => (k.encode(_k), v.encodeSplitJsonAST(_v)) })
  }

  final case class OrderedMapEncoder[K, V](k: JsonFieldEncoder[K], v: JsonEncoder[V]) extends JsonEncoder[OrderedMap[K, V]] {
    override def encodeJsonAST(value: OrderedMap[K, V]): Json =
      Json.Obj(value.elements.map { case (_k, _v) => (k.encode(_k), v.encodeJsonAST(_v)) })
    override def encodeSplitJsonAST(value: OrderedMap[K, V]): Ior[PlainTextJson, SecretJson] =
      SecretUtil.splitObjectElems(value.elements.map { case (_k, _v) => (k.encode(_k), v.encodeSplitJsonAST(_v)) })
  }

  sealed trait TupleEncoder[A <: Tuple] extends JsonEncoder[A] {

    val size: Int

    private[TupleEncoder] def append(value: A, offset: Int, array: Array[Json]): Unit
    private[TupleEncoder] def appendSplit(value: A, offset: Int, array: Array[Ior[PlainTextJson, SecretJson]]): Unit

    override final def encodeJsonAST(value: A): Json = {
      val array: Array[Json] = new Array[Json](size)
      val arraySeq: ArraySeq[Json] = ArraySeq.unsafeWrapArray(array)
      append(value, 0, array)
      Json.Arr(arraySeq)
    }

    override final def encodeSplitJsonAST(value: A): Ior[PlainTextJson, SecretJson] = {
      val array: Array[Ior[PlainTextJson, SecretJson]] = new Array[Ior[PlainTextJson, SecretJson]](size)
      val arraySeq: ArraySeq[Ior[PlainTextJson, SecretJson]] = ArraySeq.unsafeWrapArray(array)
      appendSplit(value, 0, array)
      SecretUtil.splitArrayElems(arraySeq)
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

      override private[TupleEncoder] def appendSplit(value: A *: B, offset: Int, array: Array[Ior[PlainTextJson, SecretJson]]): Unit =
        value match {
          case aValue *: bValue =>
            array(offset) = a.encodeSplitJsonAST(aValue)
            b.appendSplit(bValue, offset + 1, array)
        }

    }

    case object Empty extends TupleEncoder[EmptyTuple] {

      override val size: Int = 0

      override private[TupleEncoder] def append(value: EmptyTuple, offset: Int, array: Array[Json]): Unit =
        ()

      override private[TupleEncoder] def appendSplit(value: EmptyTuple, offset: Int, array: Array[Ior[PlainTextJson, SecretJson]]): Unit =
        ()

    }

    given emptyTuple: TupleEncoder[EmptyTuple] = TupleEncoder.Empty
    given tupleAppend: [A, B <: Tuple] => (a: JsonEncoder[A], b: TupleEncoder[B]) => TupleEncoder[A *: B] = TupleEncoder.Append(a, b)

  }

  //////////////////////////////////////////////////////////////////////////////////////////////////////
  //      Transformers
  //////////////////////////////////////////////////////////////////////////////////////////////////////

  final case class Contramapped[A, B](encoder: JsonEncoder[A], f: B => A) extends JsonEncoder[B] {
    override def encodeJsonAST(value: B): Json =
      encoder.encodeJsonAST(f(value))
    override def encodeSplitJsonAST(value: B): Ior[PlainTextJson, SecretJson] =
      encoder.encodeSplitJsonAST(f(value))
    override def addToObject(value: B): Boolean =
      encoder.addToObject(f(value))
  }

  final case class ContramappedObject[A, B](encoder: JsonEncoder.ObjectEncoder[A], f: B => A) extends JsonEncoder.ObjectEncoder[B] {
    override def encodeJsonObjectFields(value: B): Growable[(String, Json)] =
      encoder.encodeJsonObjectFields(f(value))
    override def addToObject(value: B): Boolean =
      encoder.addToObject(f(value))
    override def encodeSplitJsonAST(value: B): Ior[PlainTextJsonObject, SecretJsonObject] = encoder.encodeSplitJsonAST(f(value))
    override def encodeSplitJsonObjectFields(value: B): Growable[(String, Ior[PlainTextJson, SecretJson])] = encoder.encodeSplitJsonObjectFields(f(value))
  }

  final case class MapJsonOutput[A](encoder: JsonEncoder[A], f: Json => Json) extends JsonEncoder[A] {
    override def encodeJsonAST(value: A): Json =
      f(encoder.encodeJsonAST(value))
    override def encodeSplitJsonAST(value: A): Ior[PlainTextJson, SecretJson] =
      encoder.encodeSplitJsonAST(value).bimap(
        plain => PlainTextJson.wrap(f(plain)),
        secret => SecretJson.wrap(f(secret)),
      )
    override def addToObject(value: A): Boolean =
      encoder.addToObject(value)
  }

  //////////////////////////////////////////////////////////////////////////////////////////////////////
  //      Secret
  //////////////////////////////////////////////////////////////////////////////////////////////////////

  sealed trait Secret[A] extends JsonEncoder[A] {
    override def encodeSplitJsonAST(value: A): Ior.Right[SecretJson]
    override def contramap[B](f: B => A): JsonEncoder.Secret[B] = JsonEncoder.Secret.Contramapped(this, f)
  }
  object Secret {

    def fromJsonEncoder[A](underlying: JsonEncoder[A]): JsonEncoder.Secret[A] = underlying match
      case underlying: JsonEncoder.Secret[A]        => underlying
      case underlying: JsonEncoder.ObjectEncoder[A] => JsonEncoder.Secret.SecretizeObject(underlying)
      case underlying                               => JsonEncoder.Secret.SecretizeJson(underlying)

    def fromJsonObjectEncoder[A](underlying: JsonEncoder.ObjectEncoder[A]): JsonEncoder.Secret.ObjectEncoder[A] = underlying match
      case underlying: JsonEncoder.Secret.ObjectEncoder[A] => underlying
      case underlying                                      => JsonEncoder.Secret.SecretizeObject(underlying)

    final case class SecretizeJson[A] private[Secret] (underlying: JsonEncoder[A]) extends JsonEncoder.Secret[A] {
      override def encodeJsonAST(value: A): Json = underlying.encodeJsonAST(value)
      override def encodeSplitJsonAST(value: A): Ior.Right[SecretJson] = Ior.Right(SecretJson.wrap(encodeJsonAST(value)))
      override def addToObject(value: A): Boolean = underlying.addToObject(value)
    }

    final case class Contramapped[A, B](encoder: JsonEncoder.Secret[A], f: B => A) extends JsonEncoder.Secret[B] {
      override def encodeJsonAST(value: B): Json = encoder.encodeJsonAST(f(value))
      override def encodeSplitJsonAST(value: B): Ior.Right[SecretJson] = encoder.encodeSplitJsonAST(f(value))
      override def addToObject(value: B): Boolean = encoder.addToObject(f(value))
    }

    sealed trait ObjectEncoder[A] extends JsonEncoder.Secret[A], JsonEncoder.ObjectEncoder[A] {
      override final def encodeSplitJsonAST(value: A): Ior.Right[SecretJsonObject] = Ior.Right(SecretJsonObject.wrap(encodeJsonAST(value)))
      override def contramap[B](f: B => A): JsonEncoder.Secret.ObjectEncoder[B] = JsonEncoder.Secret.ContramappedObject(this, f)
    }

    final case class SecretizeObject[A] private[Secret] (underlying: JsonEncoder.ObjectEncoder[A]) extends JsonEncoder.Secret.ObjectEncoder[A] {
      override def encodeJsonObjectFields(value: A): Growable[(String, Json)] =
        underlying.encodeJsonObjectFields(value)
      override def encodeSplitJsonObjectFields(value: A): Growable[(String, Ior.Right[SecretJson])] =
        underlying.encodeJsonObjectFields(value).map { (key, value) => (key, Ior.Right(SecretJson.wrap(value))) }
      override def addToObject(value: A): Boolean = underlying.addToObject(value)
    }

    final case class ContramappedObject[A, B](encoder: JsonEncoder.Secret.ObjectEncoder[A], f: B => A) extends JsonEncoder.Secret.ObjectEncoder[B] {
      override def encodeJsonObjectFields(value: B): Growable[(String, Json)] = encoder.encodeJsonObjectFields(f(value))
      override def encodeSplitJsonObjectFields(value: B): Growable[(String, Ior[PlainTextJson, SecretJson])] = encoder.encodeSplitJsonObjectFields(f(value))
      override def addToObject(value: B): Boolean = encoder.addToObject(f(value))
    }
  }

  //////////////////////////////////////////////////////////////////////////////////////////////////////
  //      Generic
  //////////////////////////////////////////////////////////////////////////////////////////////////////

  override protected def productDeriver[A](using
      Quotes,
      Type[JsonEncoder.ObjectEncoder],
      Type[A],
      ProductGeneric[A],
      Derivable[JsonEncoder.ObjectEncoder],
  ): Derivable.ProductDeriver[JsonEncoder.ObjectEncoder, A] =
    Derivable.ProductDeriver.withCustomDisjointInstances[JsonEncoder.ObjectEncoder, JsonEncoder, A] { _ ?=> (generic: ProductGeneric[A]) =>
      generic.cacheVals[JsonEncoder](
        valName = n => s"instance_$n",
        valType = ValDef.ValType.LazyVal,
      ) { [b] => (_, _) ?=> (field: generic.Field[b]) =>
        val baseInstance: Expr[JsonEncoder[b]] = field.summonTypeClass[JsonEncoder]
        val isPlain: Boolean = field.annotations.optionalOf[jsonSecret].isEmpty
        if isPlain then baseInstance
        else '{ $baseInstance.secret }
      }
    } { DeriveProductJsonEncoder[A](_) }

  override protected def sumDeriver[A](using
      Quotes,
      Type[JsonEncoder.ObjectEncoder],
      Type[A],
      SumGeneric[A],
      Derivable[JsonEncoder.ObjectEncoder],
  ): Derivable.SumDeriver[JsonEncoder.ObjectEncoder, A] =
    Derivable.SumDeriver.withInstances { DeriveSumJsonEncoder[A](_) }

  override inline def derived[A]: JsonEncoder.ObjectEncoder[A] = ${ derivedImpl[A] }

  private def deriveWrappedImpl[A: Type](using Quotes): Expr[JsonEncoder[A]] = {
    type B
    val wrapping: ProductGeneric.SingleFieldCaseClassGeneric[A, B] = ProductGeneric.SingleFieldCaseClassGeneric.ofTypeField[A, B]
    given Type[B] = wrapping.field.tpe

    '{ ${ wrapping.field.summonTypeClass[JsonEncoder] }.contramap[A](${ wrapping.singleField.unwrapExpr }) }
  }

  inline def deriveWrapped[A]: JsonEncoder[A] = ${ deriveWrappedImpl[A] }

}

//////////////////////////////////////////////////////////////////////////////////////////////////////
//      Low Priority Instances
//////////////////////////////////////////////////////////////////////////////////////////////////////

object JsonEncoderLowPriority {

  trait LowPriority1 {

    given fromJsonCodec: [A] => (codec: JsonCodec[A]) => JsonEncoder[A] = codec.encoder

    given nonEmptySeq: [S1[_], S2[_], A] => (ne: NonEmpty.Aux[S1, S2]) => (encoder: JsonEncoder[S1[A]]) => JsonEncoder[S2[A]] =
      encoder.contramap(ne.eraseNonEmpty)

  }

}
