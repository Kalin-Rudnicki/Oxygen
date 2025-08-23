package oxygen.http.core.generic

import oxygen.predef.core.*
import oxygen.quoted.*
import scala.quoted.*

final class ApiRepr[Api] private (_apiTpe: Type[Api])(using Quotes) {

  given apiTpe: Type[Api] = _apiTpe
  val typeRepr: TypeRepr = TypeRepr.of[Api]
  val sym: Symbol = typeRepr.typeSymbol
  val tree: Tree = sym.tree

  if (!sym.flags.is(Flags.Trait))
    report.errorAndAbort("Api must be a trait")

  val classDef: ClassDef = tree.narrow[ClassDef]("trait is not a classDef?")

  // TODO (KR) : detect unimplemented parents
  val defDefs: ArraySeq[DefDef] =
    classDef.body.toArraySeq.map(_.narrow[DefDef]("body contains non-def-def"))

  val routes: ArraySeq[RouteRepr[Api]] = defDefs.map(RouteRepr.derive[Api](_))

  def toIndentedString: IndentedString =
    IndentedString.section(s"${typeRepr.showAnsiCode}:")(routes.map(_.toIndentedString).toSeq*)

}
object ApiRepr {

  def derive[Api: Type as apiTpe](using Quotes): ApiRepr[Api] =
    new ApiRepr[Api](apiTpe)

}
