package oxygen.executable.generic

import oxygen.cli.*
import oxygen.meta.given
import oxygen.quoted.*
import scala.quoted.*

private[generic] final class RawParamRepr(
    val valDef: ValDef,
    val parentPos: Position,
    val paramIdx: Int,
    val ownerName: Option[String],
    val defaultSyms: Map[(String, Int), Term],
)(using quotes: Quotes) {

  type T

  val valPosition: Position = valDef.symbol.pos.getOrElse(valDef.pos)

  def failAtVal(msg: String): Nothing = report.errorAndAbort(msg, valPosition)

  private val annotations: Annotations = valDef.symbol.annotations

  val annot_paramType: CliFunctionParamType = annotations.optionalOfValue[CliFunctionParamType].getOrElse { failAtVal("Missing param type annotation") }

  val annot_longName: Option[longName] = annotations.optionalOfValue[longName]
  val annot_longName_truePrefix: Option[longName.truePrefix] = annotations.optionalOfValue[longName.truePrefix]
  val annot_longName_falsePrefix: Option[longName.falsePrefix] = annotations.optionalOfValue[longName.falsePrefix]
  val annot_longName_trueName: Option[longName.trueName] = annotations.optionalOfValue[longName.trueName]
  val annot_longName_falseName: Option[longName.falseName] = annotations.optionalOfValue[longName.falseName]

  val annot_shortName: Option[shortName.Base] = annotations.optionalOfValue[shortName.Base]
  val annot_shortName_trueName: Option[shortName.trueName] = annotations.optionalOfValue[shortName.trueName]
  val annot_shortName_falseName: Option[shortName.falseName] = annotations.optionalOfValue[shortName.falseName]

  val annot_doc: Option[doc] = annotations.optionalOfValue[doc]

  val typeRepr: TypeRepr = valDef.tpt.tpe.widen
  given Type[T] = typeRepr.asTypeOf

}