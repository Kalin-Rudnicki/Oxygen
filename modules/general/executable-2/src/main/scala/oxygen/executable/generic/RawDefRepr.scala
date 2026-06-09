package oxygen.executable.generic

import oxygen.executable.*
import oxygen.predef.core.*
import oxygen.quoted.*
import scala.quoted.*

private[generic] final class RawDefRepr(val defDef: DefDef, val annot: CliFunctionAnnotation, val parentPos: Position)(using quotes: Quotes) {

  val defPosition: Position = defDef.symbol.pos.getOrElse(defDef.pos)

  def failAtDef(msg: String): Nothing = report.errorAndAbort(msg, defPosition)

  type ReturnType

  val returnTypeRepr: TypeRepr = defDef.returnTpt.tpe.dealiasKeepOpaques
  val returnType: Type[ReturnType] = returnTypeRepr.asTypeOf

  val paramClause: Option[TermParamClause] = defDef.paramss match
    case Nil                                                                       => None
    case (params: TermParamClause) :: Nil if !params.isGiven && !params.isImplicit => params.some
    case _                                                                         => failAtDef("Requires exactly 0 ir 1 parameter groups")

  val valDefs: List[ValDef] = paramClause.fold(Nil)(_.params)

  val params: List[RawParamRepr] = valDefs.map { valDef => new RawParamRepr(valDef, defPosition) }

}
