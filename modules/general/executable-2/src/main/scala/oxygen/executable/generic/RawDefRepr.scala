package oxygen.executable.generic

import oxygen.executable.*
import oxygen.predef.core.*
import oxygen.quoted.*
import scala.quoted.*

private[generic] final class RawDefRepr(
    val defDef: DefDef,
    val optAnnot: Option[CliFunctionAnnotation],
    val parentPos: Position,
    val effectTypeRepr: TypeRepr,
    val subAppTypeRepr: TypeRepr,
    val ownerName: Option[String],
    val defaultSyms: Map[(String, Int), Term],
)(using quotes: Quotes) {

  val defPosition: Position = defDef.symbol.pos.getOrElse(defDef.pos)

  def failAtDef(msg: String): Nothing = report.errorAndAbort(msg, defPosition)

  def annot: CliFunctionAnnotation = optAnnot.getOrElse { report.errorAndAbort("Internal Defect : None.get on RawDefRepr without CliFunctionAnnotation") }

  type ReturnType

  val returnTypeRepr: TypeRepr = defDef.returnTpt.tpe.dealiasKeepOpaques
  val returnType: Type[ReturnType] = returnTypeRepr.asTypeOf

  val paramClause: Option[TermParamClause] = defDef.paramss match
    case Nil                                                                       => None
    case (params: TermParamClause) :: Nil if !params.isGiven && !params.isImplicit => params.some
    case _                                                                         => failAtDef("Requires exactly 0 ir 1 parameter groups")

  val valDefs: List[ValDef] = paramClause.fold(Nil)(_.params)

  val params: List[RawParamRepr] =
    valDefs.zipWithIndex.map { (valDef, idx) => new RawParamRepr(valDef, defPosition, idx, ownerName, defaultSyms) }

  def validateEnv(envLayerTypeRepr: TypeRepr): Unit =
    if !(returnTypeRepr <:< envLayerTypeRepr) then
      failAtDef(s"def env must return ${envLayerTypeRepr.showAnsiCode}, found ${returnTypeRepr.showAnsiCode}")

  optAnnot.foreach {
    case command(_) =>
      if returnTypeRepr <:< effectTypeRepr then ()
      else if returnTypeRepr <:< subAppTypeRepr then ()
      else failAtDef(s"@command must return ${effectTypeRepr.showAnsiCode} or ${subAppTypeRepr.showAnsiCode}, found ${returnTypeRepr.showAnsiCode}")
    case execute() =>
      if !(returnTypeRepr <:< effectTypeRepr) then
        failAtDef(s"@execute must return ${effectTypeRepr.showAnsiCode}, found ${returnTypeRepr.showAnsiCode}")
    case inlineApp() => ()
  }

}