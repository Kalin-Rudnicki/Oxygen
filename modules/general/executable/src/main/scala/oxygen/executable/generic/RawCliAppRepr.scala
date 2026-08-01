package oxygen.executable.generic

import oxygen.executable.*
import oxygen.meta.given
import oxygen.predef.core.*
import oxygen.quoted.*
import scala.quoted.*
import zio.*

private[generic] final class RawCliAppRepr[A](using quotes: Quotes, val cliAppType: Type[A]) {

  type RequiredEnv
  type ProvidedEnv

  val (requiredIsEmpty, providedIsEmpty, requiredEnvTypeRepr, providedEnvTypeRepr): (Boolean, Boolean, TypeRepr, TypeRepr) = cliAppType match
    case '[CliApp[Any, Any]]                 => (true, true, TypeRepr.of[Any], TypeRepr.of[Any])
    case '[CliApp[requiredEnv, Any]]         => (false, true, TypeRepr.of[requiredEnv], TypeRepr.of[Any])
    case '[CliApp[Any, providedEnv]]         => (true, false, TypeRepr.of[Any], TypeRepr.of[providedEnv])
    case '[CliApp[requiredEnv, providedEnv]] => (false, false, TypeRepr.of[requiredEnv], TypeRepr.of[providedEnv])
    case _                                   =>
      val tpe = TypeRepr.of[A]
      report.errorAndAbort(
        s"CLI app must extend CliApp[RequiredEnv, ProvidedEnv], found ${tpe.showAnsiCode}",
        tpe.typeSymbol.pos.getOrElse(Position.ofMacroExpansion),
      )

  given requiredEnvType: Type[RequiredEnv] = requiredEnvTypeRepr.asTypeOf
  given providedEnvType: Type[ProvidedEnv] = providedEnvTypeRepr.asTypeOf

  // TODO (KR) : these need to match [[CliApp]]
  type FullEnv = ProvidedEnv & RequiredEnv
  type Effect = RIO[FullEnv & Scope, Unit | ExitCode]
  type EnvLayer = RLayer[RequiredEnv, ProvidedEnv]
  type SubApp = CliApp[? <: FullEnv, ?]

  val fullEnvTypeRepr: TypeRepr = TypeRepr.of[FullEnv].simplified
  val effectTypeRepr: TypeRepr = TypeRepr.of[Effect].simplified
  val envLayerTypeRepr: TypeRepr = TypeRepr.of[EnvLayer].simplified
  val subAppTypeRepr: TypeRepr = TypeRepr.of[SubApp].simplified

  // Taken straight off the type symbol (mirroring how `Generic.of` starts) rather than via
  // `ProductGeneric`, so a non-case-class sub-app (trait / abstract class) doesn't trip a product
  // derivation. Only the *root* instantiation path needs `ProductGeneric` (see `DeriveCliApp.buildRootApp`).
  val typeSymbol: Symbol = TypeRepr.of[A].typeSymbol

  val pos: Position = typeSymbol.pos.getOrElse(Position.ofMacroExpansion)

  val classDef: ClassDef =
    typeSymbol.tree.narrowOpt[ClassDef].getOrElse { report.errorAndAbort(s"CLI app ${typeSymbol.name} must be a class or trait", pos) }

  private val defaultReg = "^([^$]+)\\$default\\$(\\d+)$".r
  val defaultSyms: Map[(String, Int), Term] =
    typeSymbol.declaredMethods.flatMap { sym =>
      sym.name match
        case defaultReg(name, idx) =>
          val defDef: DefDef = sym.tree.narrow[DefDef]
          val rhs: Term = defDef.rhs.getOrElse { report.errorAndAbort(s"Missing default value for parameter $name", sym.pos.getOrElse(pos)) }
          ((name, idx.toInt - 1), rhs).some
        case _ => None
    }.toMap

  val bodyDefs: List[RawDefRepr] =
    for {
      statement <- classDef.body
      defDef <- statement.narrowOpt[DefDef]
      annot <- defDef.symbol.annotations.optionalOfValue[CliFunctionAnnotation]
    } yield new RawDefRepr(defDef, annot.some, pos, effectTypeRepr, subAppTypeRepr, fullEnvTypeRepr, defDef.name.some, defaultSyms)

  val envDef: Option[RawDefRepr] =
    (for {
      statement <- classDef.body
      defDef <- statement.narrowOpt[DefDef] if defDef.name == "env"
    } yield new RawDefRepr(defDef, None, pos, effectTypeRepr, subAppTypeRepr, fullEnvTypeRepr, "env".some, defaultSyms)) match
      case head :: Nil =>
        head.validateEnv(envLayerTypeRepr)
        head.some
      case Nil => None
      case _   => report.errorAndAbort("Only one `def env` is allowed per CLI app", pos)

  val commandDefs: List[RawDefRepr] = bodyDefs.filter(_.optAnnot.exists(_.isInstanceOf[command]))
  val executeDefs: List[RawDefRepr] = bodyDefs.filter(_.optAnnot.exists(_.isInstanceOf[execute]))

  if commandDefs.nonEmpty && executeDefs.nonEmpty then
    report.errorAndAbort("Cannot have both @command and @execute in the same class", pos)

  if executeDefs.size > 1 then
    report.errorAndAbort("Only one @execute is allowed", pos)

}
