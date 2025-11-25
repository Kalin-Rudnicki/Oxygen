package oxygen.schema.compat

import oxygen.json.Json
import oxygen.predef.core.*
import oxygen.schema.NumberFormat
import oxygen.schema.compiled.*

sealed trait ComparisonResult {

  def toIndentedStringNoTypes: IndentedString

  def pruned: ComparisonResult

  final lazy val toOption: Option[ComparisonResult.Different] = this match
    case self: ComparisonResult.Different => self.some
    case _                                => None

  final def isDifferent: Boolean = toOption.nonEmpty

}
object ComparisonResult {

  /////// General ///////////////////////////////////////////////////////////////

  final case class RecursiveReference(from: CompiledSchemaRef, to: CompiledSchemaRef) extends ComparisonResult {

    override def toIndentedStringNoTypes: IndentedString =
      IndentedString.keyValueSection("RecursiveReference:")(
        "from: " -> from.showCore,
        "to: " -> to.showCore,
      )

    override def pruned: ComparisonResult = this

  }

  sealed trait Concrete extends ComparisonResult {
    val from: FullCompiledSchema
    val to: FullCompiledSchema

    protected final def toExactEqual: ComparisonResult.ExactEqual = ComparisonResult.ExactEqual(from, to)

  }

  final case class ExactEqual(from: FullCompiledSchema, to: FullCompiledSchema) extends ComparisonResult.Concrete {

    override def toIndentedStringNoTypes: IndentedString = "ExactEqual"

    override def pruned: ComparisonResult = this

  }

  sealed trait Different extends ComparisonResult.Concrete

  final case class FromIsMoreSpecific(from: FullCompiledSchema, to: FullCompiledSchema) extends ComparisonResult.Different {

    override def toIndentedStringNoTypes: IndentedString =
      IndentedString.keyValueSection("FromIsMoreSpecific:")(
        "from: " -> from.toIndentedString,
        "to: " -> to.toIndentedString,
      )

    override def pruned: ComparisonResult = this

  }

  final case class ToIsMoreSpecific(from: FullCompiledSchema, to: FullCompiledSchema) extends ComparisonResult.Different {

    override def toIndentedStringNoTypes: IndentedString =
      IndentedString.keyValueSection("ToIsMoreSpecific:")(
        "from: " -> from.toIndentedString,
        "to: " -> to.toIndentedString,
      )

    override def pruned: ComparisonResult = this

  }

  final case class NotComparable(from: FullCompiledSchema, to: FullCompiledSchema) extends ComparisonResult.Different {

    override def toIndentedStringNoTypes: IndentedString =
      IndentedString.keyValueSection("NotComparable:")(
        "from: " -> from.toIndentedString,
        "to: " -> to.toIndentedString,
      )

    override def pruned: ComparisonResult = this

  }

  /////// Specific ///////////////////////////////////////////////////////////////

  final case class FormattedText(
      from: FullCompiledPlainSchema.FormattedText,
      to: FullCompiledPlainSchema.FormattedText,
      formats: AddedRemovedBoth.Many[String, String],
      underlying: ComparisonResult,
  ) extends ComparisonResult.Different {

    override def toIndentedStringNoTypes: IndentedString =
      IndentedString.keyValueSection("FormattedText:")(
        "formats: " -> formats.toIndentedString(s => s"- $s", s => s"- $s"),
        "underlying: " -> underlying.toIndentedStringNoTypes,
      )

    override def pruned: ComparisonResult = {
      val formats2 = formats.prune(_ => None)
      val underlying2 = underlying.pruned
      val different = formats2.nonEmpty || underlying2.isDifferent
      if different then
        FormattedText(
          from = from,
          to = to,
          formats = formats2,
          underlying = underlying2,
        )
      else toExactEqual
    }

  }

  final case class Enum(
      from: FullCompiledPlainSchema.Enum,
      to: FullCompiledPlainSchema.Enum,
      caseSensitive: FromToValues[Boolean],
      exhaustive: FromToValues[Boolean],
      values: AddedRemovedBoth.Many[String, String],
  ) extends ComparisonResult.Different {

    override def toIndentedStringNoTypes: IndentedString =
      IndentedString.keyValueSection("Enum:")(
        "caseSensitive: " -> caseSensitive.toIndentedString(_.toString),
        "exhaustive: " -> exhaustive.toIndentedString(_.toString),
        "values: " -> values.toIndentedString(s => s"- $s", s => s"- $s"),
      )

    override def pruned: ComparisonResult = {
      val values2 = values.prune(_ => None)
      val different = caseSensitive.isDifferent || exhaustive.isDifferent || values2.nonEmpty
      if different then
        Enum(
          from = from,
          to = to,
          caseSensitive = caseSensitive,
          exhaustive = exhaustive,
          values = values2,
        )
      else toExactEqual
    }

  }

  final case class BearerToken(
      from: FullCompiledPlainSchema.BearerToken,
      to: FullCompiledPlainSchema.BearerToken,
      underlying: ComparisonResult,
  ) extends ComparisonResult.Different {

    override def toIndentedStringNoTypes: IndentedString =
      IndentedString.keyValueSection("BearerToken:")(
        "underlying: " -> underlying.toIndentedStringNoTypes,
      )

    override def pruned: ComparisonResult = {
      val underlying2 = underlying.pruned
      val different = underlying2.isDifferent
      if different then
        BearerToken(
          from = from,
          to = to,
          underlying = underlying2,
        )
      else toExactEqual
    }

  }

  final case class EncodedText(
      from: FullCompiledPlainSchema.EncodedText,
      to: FullCompiledPlainSchema.EncodedText,
      encoding: FromToValues[String],
      underlying: ComparisonResult,
  ) extends ComparisonResult.Different {

    override def toIndentedStringNoTypes: IndentedString =
      IndentedString.keyValueSection("EncodedText:")(
        "encoding: " -> encoding.toIndentedString(identity(_)),
        "underlying: " -> underlying.toIndentedStringNoTypes,
      )

    override def pruned: ComparisonResult = {
      val underlying2 = underlying.pruned
      val different = encoding.isDifferent || underlying2.isDifferent
      if different then
        EncodedText(
          from = from,
          to = to,
          encoding = encoding,
          underlying = underlying2,
        )
      else toExactEqual
    }

  }

  final case class JsonNumber(
      from: FullCompiledJsonSchema.JsonNumber,
      to: FullCompiledJsonSchema.JsonNumber,
      numberFormat: FromToValues[NumberFormat],
  ) extends ComparisonResult.Different {

    override def toIndentedStringNoTypes: IndentedString =
      IndentedString.keyValueSection("JsonNumber:")(
        "numberFormat: " -> numberFormat.toIndentedString(_.toString),
      )

    override def pruned: ComparisonResult = {
      val different = numberFormat.isDifferent
      if different then
        JsonNumber(
          from = from,
          to = to,
          numberFormat = numberFormat,
        )
      else toExactEqual
    }

  }

  final case class JsonAST(
      from: FullCompiledJsonSchema.JsonAST,
      to: FullCompiledJsonSchema.JsonAST,
      jsonType: FromToValues[Json.Type],
  ) extends ComparisonResult.Different {

    override def toIndentedStringNoTypes: IndentedString =
      IndentedString.keyValueSection("JsonAST:")(
        "jsonType: " -> jsonType.toIndentedString(_.toString),
      )

    override def pruned: ComparisonResult = {
      val different = jsonType.isDifferent
      if different then
        JsonAST(
          from = from,
          to = to,
          jsonType = jsonType,
        )
      else toExactEqual
    }

  }

  final case class JsonString(
      from: FullCompiledJsonSchema.JsonString,
      to: FullCompiledJsonSchema.JsonString,
      underlying: ComparisonResult,
  ) extends ComparisonResult.Different {

    override def toIndentedStringNoTypes: IndentedString =
      IndentedString.keyValueSection("JsonString:")(
        "underlying: " -> underlying.toIndentedStringNoTypes,
      )

    override def pruned: ComparisonResult = {
      val underlying2 = underlying.pruned
      val different = underlying2.isDifferent
      if different then
        JsonString(
          from = from,
          to = to,
          underlying = underlying2,
        )
      else toExactEqual
    }

  }

  final case class JsonArray(
      from: FullCompiledJsonSchema.JsonArray,
      to: FullCompiledJsonSchema.JsonArray,
      underlying: ComparisonResult,
  ) extends ComparisonResult.Different {

    override def toIndentedStringNoTypes: IndentedString =
      IndentedString.keyValueSection("JsonArray:")(
        "underlying: " -> underlying.toIndentedStringNoTypes,
      )

    override def pruned: ComparisonResult = {
      val underlying2 = underlying.pruned
      val different = underlying2.isDifferent
      if different then
        JsonArray(
          from = from,
          to = to,
          underlying = underlying2,
        )
      else toExactEqual
    }

  }

  final case class JsonMap(
      from: FullCompiledJsonSchema.JsonMap,
      to: FullCompiledJsonSchema.JsonMap,
      keyUnderlying: ComparisonResult,
      valueUnderlying: ComparisonResult,
  ) extends ComparisonResult.Different {

    override def toIndentedStringNoTypes: IndentedString =
      IndentedString.keyValueSection("JsonMap:")(
        "key: " -> keyUnderlying.toIndentedStringNoTypes,
        "value: " -> valueUnderlying.toIndentedStringNoTypes,
      )

    override def pruned: ComparisonResult = {
      val key2 = keyUnderlying.pruned
      val value2 = valueUnderlying.pruned
      val different = key2.isDifferent || value2.isDifferent
      if different then
        JsonMap(
          from = from,
          to = to,
          keyUnderlying = key2,
          valueUnderlying = value2,
        )
      else toExactEqual
    }

  }

  final case class JsonProduct(
      from: FullCompiledJsonSchema.JsonProduct,
      to: FullCompiledJsonSchema.JsonProduct,
      fields: AddedRemovedBoth.Many[FullCompiledJsonSchema.ProductField, FieldComparison],
  ) extends ComparisonResult.Different {

    override def toIndentedStringNoTypes: IndentedString =
      IndentedString.keyValueSection("JsonProduct:")(
        "fields: " -> fields.toIndentedString(productFieldToIndentedString, _.toIndentedString),
      )

    override def pruned: ComparisonResult = {
      val fields2 = fields.prune { field =>
        val underlying2 = field.typeComparison.pruned
        val different = field.nullable.isDifferent || field.onMissing.isDifferent || underlying2.isDifferent
        Option.when(different) {
          FieldComparison(
            fieldName = field.fieldName,
            nullable = field.nullable,
            onMissing = field.onMissing,
            typeComparison = underlying2,
          )
        }
      }
      val different = fields2.nonEmpty
      if different then
        JsonProduct(
          from = from,
          to = to,
          fields = fields2,
        )
      else toExactEqual
    }

  }

  final case class JsonSum(
      from: FullCompiledJsonSchema.JsonSum,
      to: FullCompiledJsonSchema.JsonSum,
      discriminator: FromToValues[Option[String]],
      cases: AddedRemovedBoth.Many[FullCompiledJsonSchema.SumCase, CaseComparison],
  ) extends ComparisonResult.Different {

    override def toIndentedStringNoTypes: IndentedString =
      IndentedString.keyValueSection("JsonSum:")(
        "discriminator: " -> discriminator.toIndentedString(_.getOrElse("[N/A]")),
        "cases: " -> cases.toIndentedString(sumCaseToIndentedString, _.toIndentedString),
      )

    override def pruned: ComparisonResult = {
      val cases2 = cases.prune { kase =>
        val underlying2 = kase.typeComparison.pruned
        val different = underlying2.isDifferent
        Option.when(different) {
          CaseComparison(
            caseName = kase.caseName,
            typeComparison = underlying2,
          )
        }
      }
      val different = discriminator.isDifferent || cases2.nonEmpty
      if different then
        JsonSum(
          from = from,
          to = to,
          discriminator = discriminator,
          cases = cases2,
        )
      else toExactEqual
    }
  }

  final case class Transformed(
      from: FullCompiledSchema,
      to: FullCompiledSchema,
      transforms: FromToValues[List[RawCompiledSchema.SourceFile]],
      underlying: ComparisonResult,
  ) extends ComparisonResult.Different {

    override def toIndentedStringNoTypes: IndentedString =
      IndentedString.keyValueSection("JsonArray:")(
        "transforms: " -> transforms.toIndentedString { _.map { f => s"- $f" } },
        "underlying: " -> underlying.toIndentedStringNoTypes,
      )

    override def pruned: ComparisonResult = {
      val underlying2 = underlying.pruned
      val different = transforms.isDifferent || underlying2.isDifferent
      if different then
        Transformed(
          from = from,
          to = to,
          transforms = transforms,
          underlying = underlying2,
        )
      else toExactEqual
    }

  }

  /////// Field:Case ///////////////////////////////////////////////////////////////

  private def productFieldToIndentedString(field: FullCompiledJsonSchema.ProductField): IndentedString =
    IndentedString.keyValueSection(field.fieldName + ":")(
      "nullable: " -> field.nullable.toString,
      "onMissing: " -> field.onMissing.fold("<decode-error>")(_.toString),
      "type: " -> field.fieldType.value.toIndentedString,
    )

  private def sumCaseToIndentedString(kase: FullCompiledJsonSchema.SumCase): IndentedString =
    IndentedString.keyValueSection(kase.caseName + ":")(
      "type: " -> kase.caseType.value.toIndentedString,
    )

  final case class FieldComparison(
      fieldName: String,
      nullable: FromToValues[Boolean],
      onMissing: FromToValues[Option[RawCompiledJsonSchema.ProductField.DecodeMissingAs]],
      typeComparison: ComparisonResult,
  ) {

    def toIndentedString: IndentedString =
      IndentedString.keyValueSection(fieldName + ":")(
        "nullable: " -> nullable.toIndentedString(_.toString),
        "onMissing: " -> onMissing.toIndentedString(_.fold("<decode-error>")(_.toString)),
        "underlying: " -> typeComparison.toIndentedStringNoTypes,
      )

  }

  final case class CaseComparison(
      caseName: String,
      typeComparison: ComparisonResult,
  ) {

    def toIndentedString: IndentedString =
      IndentedString.keyValueSection(caseName + ":")(
        "underlying: " -> typeComparison.toIndentedStringNoTypes,
      )

  }

}
