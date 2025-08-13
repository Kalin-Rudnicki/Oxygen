package oxygen.sql.schema

import java.time.*
import java.util.UUID
import oxygen.json.JsonCodec
import oxygen.predef.test.*
import oxygen.sql.model.*

object RowSchemaSpec extends OxygenSpecDefault {

  final case class Product1(
      key: UUID,
      value: String,
      verified: Option[Boolean],
  ) derives RowRepr.ProductRepr,
        JsonCodec

  final case class Product2(
      @columnName("custom") column1: Product1,
      column2: Option[Product1],
  ) derives RowRepr.ProductRepr

  final case class Product3(
      @inlineColumnNames nested: Product2,
  ) derives RowRepr.ProductRepr

  private def singleColumnTest[A](
      expType: Column.Type,
  )(using nonNullSchema: RowRepr[A], nullSchema: RowRepr[Option[A]], tag: TypeTag[A]): TestSpec =
    test(tag.prefixNone) {
      val prefixedNonNull = nonNullSchema.prefixed("prefix")
      val prefixedInlineNonNull = nonNullSchema.prefixedInline("prefix")
      val prefixedNull = nullSchema.prefixed("prefix")
      val prefixedInlineNull = nullSchema.prefixedInline("prefix")

      assertTrue(
        nonNullSchema.columns.columns == ArraySeq(Column("", expType, false)),
        prefixedNonNull.columns.columns == ArraySeq(Column("prefix", expType, false)),
        prefixedInlineNonNull.columns.columns == ArraySeq(Column("prefix", expType, false)),
        nullSchema.columns.columns == ArraySeq(Column("", expType, true)),
        prefixedNull.columns.columns == ArraySeq(Column("prefix", expType, true)),
        prefixedInlineNull.columns.columns == ArraySeq(Column("prefix", expType, true)),
      )
    }

  private def productColumnTest[A](
      columns: Column*,
  )(using schema: RowRepr.ProductRepr[A], tag: TypeTag[A]): TestSpec =
    test(tag.prefixNone) {
      assert(schema.columns.columns)(equalTo_filteredDiff(columns.toArraySeq)) &&
      assert(schema.prefixedInline("prefix").columns.columns)(equalTo_filteredDiff(columns.toArraySeq))
    }

  private def singleColumnsSpec: TestSpec =
    suite("single")(
      singleColumnTest[Short](Column.Type.SmallInt),
      singleColumnTest[Int](Column.Type.Int),
      singleColumnTest[Long](Column.Type.BigInt),
      singleColumnTest[Float](Column.Type.Real),
      singleColumnTest[Double](Column.Type.DoublePrecision),
      singleColumnTest[String](Column.Type.Text),
      singleColumnTest[Instant](Column.Type.ZonedTimestamp),
      singleColumnTest[LocalDateTime](Column.Type.Timestamp),
      singleColumnTest[LocalDate](Column.Type.Date),
      singleColumnTest[LocalTime](Column.Type.Time),
      singleColumnTest[Boolean](Column.Type.Boolean),
      singleColumnTest[UUID](Column.Type.UUID),
      singleColumnTest[Json](Column.Type.Json),
      singleColumnTest[TypedJson[Int]](Column.Type.Json),
      singleColumnTest[TypedJson[Product1]](Column.Type.Json),
      singleColumnTest[Jsonb](Column.Type.Jsonb),
      singleColumnTest[TypedJsonb[Int]](Column.Type.Jsonb),
      singleColumnTest[TypedJsonb[Product1]](Column.Type.Jsonb),
    )

  private def productColumnsSpec: TestSpec =
    suite("product")(
      productColumnTest[Product1](
        Column("key", Column.Type.UUID, false),
        Column("value", Column.Type.Text, false),
        Column("verified", Column.Type.Boolean, true),
      ),
      productColumnTest[Product2](
        Column("custom_key", Column.Type.UUID, false),
        Column("custom_value", Column.Type.Text, false),
        Column("custom_verified", Column.Type.Boolean, true),
        Column("column_2_key", Column.Type.UUID, true),
        Column("column_2_value", Column.Type.Text, true),
        Column("column_2_verified", Column.Type.Boolean, true),
      ),
      productColumnTest[Product3](
        Column("custom_key", Column.Type.UUID, false),
        Column("custom_value", Column.Type.Text, false),
        Column("custom_verified", Column.Type.Boolean, true),
        Column("column_2_key", Column.Type.UUID, true),
        Column("column_2_value", Column.Type.Text, true),
        Column("column_2_verified", Column.Type.Boolean, true),
      ),
    )

  private def columnsSpec: TestSpec =
    suite("columns")(
      singleColumnsSpec,
      productColumnsSpec,
    )

  override def testSpec: TestSpec =
    suite("RowSchemaSpec")(
      columnsSpec,
    )

}
