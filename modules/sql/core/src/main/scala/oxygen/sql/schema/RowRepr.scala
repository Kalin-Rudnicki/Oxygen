package oxygen.sql.schema

import java.time.*
import java.util.UUID
import org.postgresql.util.PGobject
import oxygen.meta.*
import oxygen.predef.core.*
import oxygen.predef.json.*
import oxygen.sql.error.*
import oxygen.sql.generic.typeclass.*
import scala.quoted.*

trait RowRepr[A] {

  // =====|  |=====

  val columns: Columns[A]
  val decoder: ResultDecoder[A]
  val encoder: InputEncoder[A]

  def decoderWithColumns: ResultDecoder[A] = ResultDecoder.WithColumns(decoder, columns)

  // =====|  |=====

  /**
    * For all columns in this schema, prefix the column name with the given prefix.
    */
  def prefixed(prefix: String): RowRepr[A]

  /**
    * For all columns in this schema, prefix the column name with the given prefix.
    * Unless this schema is a product schema, in which case, no prefix will be added.
    */
  def prefixedInline(prefix: String): RowRepr[A]

  // =====|  |=====

  final def optional: RowRepr[Option[A]] = RowRepr.OptionalRepr(this)

  final def transform[B](ab: A => B, ba: B => A): RowRepr[B] = RowRepr.Transform(this, ab, ba)
  final def transformOrFail[B](ab: A => Either[String, B], ba: B => A): RowRepr[B] = RowRepr.TransformOrFail(this, ab, ba)

  final def toIndentedString: IndentedString =
    IndentedString.section("RowSchema:")(
      IndentedString.section("columns:")(
        columns.columns.map { c => s"- $c" },
      ),
    )

  override final def toString: String =
    toIndentedString.toString

  // =====|  |=====

  final def unsafeChild[B](name: String): RowRepr[B] = this match
    case self: RowRepr.ProductRepr[A] => self.productSchemaMap.getOrElse(name, throw QueryError.DSLError.NoSuchChild(self, name)).asInstanceOf[RowRepr[B]]
    case self                         => throw QueryError.DSLError.NotAProductSchema(self)

  final def unsafeRequired[B]: RowRepr[B] = this match
    case RowRepr.OptionalRepr(inner) => inner.asInstanceOf[RowRepr[B]]
    case self                        => throw QueryError.DSLError.NotAnOptionSchema(self)

}
object RowRepr {

  inline def apply[A](using ev: RowRepr[A]): RowRepr[A] = ev

  final case class ColumnRepr[A] private (
      column: Column,
      decoder: ResultDecoder.ColumnDecoder[A],
      encoder: InputEncoder.ColumnEncoder[A],
  ) extends RowRepr[A] {
    override val columns: Columns[A] = Columns(ArraySeq(column))
    override def prefixed(prefix: String): RowRepr[A] = {
      val newColumn: Column = column.prefixed(prefix)
      ColumnRepr(newColumn, decoder.withColumn(newColumn), encoder.withColumn(newColumn))
    }
    override def prefixedInline(prefix: String): RowRepr[A] = prefixed(prefix)
  }
  object ColumnRepr {

    def simplePF[A](columnType: Column.Type, decodeSingle: PartialFunction[Matchable, A], encodeSingle: A => Any): ColumnRepr[A] = {
      val column: Column = Column("", columnType, false)
      ColumnRepr(column, ResultDecoder.ColumnDecoder.makeSimplePF(column, decodeSingle), InputEncoder.ColumnEncoder.make(column, encodeSingle))
    }

  }

  final case class OptionalRepr[A](inner: RowRepr[A]) extends RowRepr[Option[A]] {

    override val columns: Columns[Option[A]] =
      Columns(inner.columns.columns.map(_.copy(nullable = true)))

    override val decoder: ResultDecoder[Option[A]] =
      ResultDecoder.OptionalDecoder(inner.decoder)

    override val encoder: InputEncoder[Option[A]] =
      InputEncoder.OptionalEncoder(inner.encoder)

    override def prefixed(prefix: String): RowRepr[Option[A]] = OptionalRepr(inner.prefixed(prefix))

    override def prefixedInline(prefix: String): RowRepr[Option[A]] = OptionalRepr(inner.prefixedInline(prefix))

  }

  trait ProductRepr[A] extends RowRepr[A] { self =>

    val productFields: ArraySeq[(String, RowRepr[?])]
    private[sql] final lazy val productSchemaMap: Map[String, RowRepr[?]] = productFields.toMap

    override final def prefixed(prefix: String): RowRepr.ProductRepr[A] =
      new ProductRepr[A] {
        override val productFields: ArraySeq[(String, RowRepr[?])] = self.productFields.map { case (k, v) => (k, v.prefixed(prefix)) }
        override val columns: Columns[A] = Columns(productFields.flatMap(_._2.columns.columns))
        override val decoder: ResultDecoder[A] = self.decoder
        override val encoder: InputEncoder[A] = self.encoder
      }

    override final def prefixedInline(prefix: String): RowRepr.ProductRepr[A] = this

  }
  object ProductRepr extends K0.Derivable[ProductRepr] {

    override protected def productDeriver[A](using Quotes, Type[ProductRepr], Type[A], K0.ProductGeneric[A], K0.Derivable[ProductRepr]): K0.Derivable.ProductDeriver[ProductRepr, A] =
      K0.Derivable.ProductDeriver.impl { DeriveProductRowRepr.populateFields[A].defineAndUse { DeriveProductRowRepr(_).derive } }

    override protected def sumDeriver[A](using Quotes, Type[ProductRepr], Type[A], K0.SumGeneric[A], K0.Derivable[ProductRepr]): K0.Derivable.SumDeriver[ProductRepr, A] =
      K0.Derivable.SumDeriver.notSupported

    override inline def derived[A]: RowRepr.ProductRepr[A] = ${ derivedImpl[A] }

  }

  final case class Transform[A, B](initial: RowRepr[A], ab: A => B, ba: B => A) extends RowRepr[B] {
    override val columns: Columns[B] = Columns(initial.columns.columns)
    override val decoder: ResultDecoder[B] = initial.decoder.map(ab)
    override val encoder: InputEncoder[B] = initial.encoder.contramap(ba)
    override def prefixed(prefix: String): RowRepr[B] = Transform(initial.prefixed(prefix), ab, ba)
    override def prefixedInline(prefix: String): RowRepr[B] = Transform(initial.prefixedInline(prefix), ab, ba)
  }

  final case class TransformOrFail[A, B](initial: RowRepr[A], ab: A => Either[String, B], ba: B => A) extends RowRepr[B] {
    override val columns: Columns[B] = Columns(initial.columns.columns)
    override val decoder: ResultDecoder[B] = initial.decoder.mapOrFail(ab)
    override val encoder: InputEncoder[B] = initial.encoder.contramap(ba)
    override def prefixed(prefix: String): RowRepr[B] = TransformOrFail(initial.prefixed(prefix), ab, ba)
    override def prefixedInline(prefix: String): RowRepr[B] = TransformOrFail(initial.prefixedInline(prefix), ab, ba)
  }

  case object Empty extends RowRepr[Unit] {
    override val columns: Columns[Unit] = Columns(ArraySeq.empty[Column])
    override val decoder: ResultDecoder[Unit] = ResultDecoder.Empty
    override val encoder: InputEncoder[Unit] = InputEncoder.Empty
    override def prefixed(prefix: String): RowRepr[Unit] = this
    override def prefixedInline(prefix: String): RowRepr[Unit] = this
  }

  //////////////////////////////////////////////////////////////////////////////////////////////////////
  //      Givens
  //////////////////////////////////////////////////////////////////////////////////////////////////////

  // =====| Numeric Types |=====

  // TODO (KR) : byte

  given short: RowRepr[Short] =
    RowRepr.ColumnRepr.simplePF(
      Column.Type.SmallInt,
      { case value: java.lang.Short => value },
      java.lang.Short.valueOf(_),
    )

  given int: RowRepr[Int] =
    RowRepr.ColumnRepr.simplePF(
      Column.Type.Int,
      { case value: java.lang.Integer => value },
      java.lang.Integer.valueOf(_),
    )

  given long: RowRepr[Long] =
    RowRepr.ColumnRepr.simplePF(
      Column.Type.BigInt,
      { case value: java.lang.Long => value },
      java.lang.Long.valueOf(_),
    )

  // TODO (KR) : bigInt

  given float: RowRepr[Float] =
    RowRepr.ColumnRepr.simplePF(
      Column.Type.Real,
      { case value: java.lang.Float => value },
      java.lang.Float.valueOf(_),
    )

  given double: RowRepr[Double] =
    RowRepr.ColumnRepr.simplePF(
      Column.Type.DoublePrecision,
      { case value: java.lang.Double => value },
      java.lang.Double.valueOf(_),
    )

  // TODO (KR) : bigDecimal

  // =====| Character Types |=====

  given string: RowRepr[String] =
    RowRepr.ColumnRepr.simplePF(
      Column.Type.Text,
      { case value: String => value },
      identity,
    )

  // =====| Date/Time Types |=====

  given instant: RowRepr[Instant] =
    RowRepr.ColumnRepr.simplePF(
      Column.Type.ZonedTimestamp,
      { case value: java.sql.Timestamp => value.toInstant },
      java.sql.Timestamp.from(_),
    )

  // TODO (KR) : offsetDateTime

  // TODO (KR) : zonedDateTime

  given localTime: RowRepr[LocalTime] =
    RowRepr.ColumnRepr.simplePF(
      Column.Type.Time,
      { case value: java.sql.Time => value.toLocalTime },
      java.sql.Time.valueOf(_),
    )

  given localDate: RowRepr[LocalDate] =
    RowRepr.ColumnRepr.simplePF(
      Column.Type.Date,
      { case value: java.sql.Date => value.toLocalDate },
      java.sql.Date.valueOf(_),
    )

  given localDateTime: RowRepr[LocalDateTime] =
    RowRepr.ColumnRepr.simplePF(
      Column.Type.Timestamp,
      { case value: java.sql.Timestamp => value.toLocalDateTime },
      java.sql.Timestamp.valueOf(_),
    )

  // =====| Boolean Types |=====

  given boolean: RowRepr[Boolean] =
    RowRepr.ColumnRepr.simplePF(
      Column.Type.Boolean,
      { case value: java.lang.Boolean => value },
      java.lang.Boolean.valueOf(_),
    )

  // =====| UUID Types |=====

  given uuid: RowRepr[UUID] =
    RowRepr.ColumnRepr.simplePF(
      Column.Type.UUID,
      { case value: UUID => value },
      identity,
    )

  // =====| Json Types |=====

  given json: RowRepr[oxygen.sql.model.Json] =
    RowRepr.ColumnRepr.simplePF(
      Column.Type.Json,
      { case value: PGobject => oxygen.sql.model.Json(value.getValue) }, // TODO (KR) : enforce pg type?
      { value =>
        val obj = new PGobject()
        obj.setType("JSON")
        obj.setValue(value.value)
        obj
      },
    )

  given jsonb: RowRepr[oxygen.sql.model.Jsonb] =
    RowRepr.ColumnRepr.simplePF(
      Column.Type.Jsonb,
      { case value: PGobject => oxygen.sql.model.Jsonb(value.getValue) }, // TODO (KR) : enforce pg type?
      { value =>
        val obj = new PGobject()
        obj.setType("JSONB")
        obj.setValue(value.value)
        obj
      },
    )

  given typedJson: [A] => (codec: JsonCodec[A]) => RowRepr[oxygen.sql.model.TypedJson[A]] =
    json.transformOrFail(
      _.value.fromJsonString[A].bimap(_.toString, oxygen.sql.model.TypedJson(_)),
      value => oxygen.sql.model.Json(value.value.toJsonStringCompact),
    )

  given typedJsonb: [A] => (codec: JsonCodec[A]) => RowRepr[oxygen.sql.model.TypedJsonb[A]] =
    jsonb.transformOrFail(
      _.value.fromJsonString[A].bimap(_.toString, oxygen.sql.model.TypedJsonb(_)),
      value => oxygen.sql.model.Jsonb(value.value.toJsonStringCompact),
    )

  // =====| Binary Types |=====

  // TODO (KR) :

  // =====| Array Types |=====

  // TODO (KR) :

  // =====| Other |=====

  given option: [A] => (inner: RowRepr[A]) => RowRepr[Option[A]] =
    RowRepr.OptionalRepr(inner)

}
