package oxygen.executable.generic

import oxygen.executable.*
import oxygen.meta.given
import oxygen.meta.k0.*
import oxygen.predef.core.*
import oxygen.quoted.*
import scala.quoted.*
import zio.*

private[generic] final class RawCliAppRepr[A](val isRoot: Boolean)(using quotes: Quotes, val cliAppType: Type[A]) {

  type RequiredEnv
  type ProvidedEnv

  val (requiredIsEmpty, providedIsEmpty, requiredEnvTypeRepr, providedEnvTypeRepr): (Boolean, Boolean, TypeRepr, TypeRepr) = cliAppType match
    case '[CliApp[Any, Any]]                 => (true, true, TypeRepr.of[Any], TypeRepr.of[Any])
    case '[CliApp[requiredEnv, Any]]         => (false, true, TypeRepr.of[requiredEnv], TypeRepr.of[Any])
    case '[CliApp[Any, providedEnv]]         => (true, false, TypeRepr.of[Any], TypeRepr.of[providedEnv])
    case '[CliApp[requiredEnv, providedEnv]] => (false, false, TypeRepr.of[requiredEnv], TypeRepr.of[providedEnv])
    case _                                   => report.errorAndAbort("")

  given requiredEnvType: Type[RequiredEnv] = requiredEnvTypeRepr.asTypeOf
  given providedEnvType: Type[ProvidedEnv] = providedEnvTypeRepr.asTypeOf

  val gen: ProductGeneric.CaseClassGeneric[A] = ProductGeneric.CaseClassGeneric.of[A]

  val typeSymbol: Symbol = gen.typeRepr.typeSymbol

  val classDef: ClassDef =
    typeSymbol.tree.narrowOpt[ClassDef].getOrElse { report.errorAndAbort("Not a ClassDef?", gen.pos) }

  val constructorParams: List[RawParamRepr] =
    if isRoot then gen.fields.toList.map { f => new RawParamRepr(f.constructorValDef, gen.pos) } else Nil

  val bodyDefs: List[RawDefRepr] =
    for {
      statement <- classDef.body
      defDef <- statement.narrowOpt[DefDef]
      annot <- defDef.symbol.annotations.optionalOfValue[CliFunctionAnnotation]
    } yield new RawDefRepr(defDef, annot.some, gen.pos)

  val envDef: Option[RawDefRepr] =
    (for {
      statement <- classDef.body
      defDef <- statement.narrowOpt[DefDef] if defDef.name == "env"
    } yield new RawDefRepr(defDef, None, gen.pos)) match
      case head :: Nil => head.some
      case Nil         => None
      case _           => report.errorAndAbort("Multiple `def env` not allowed...")

  // TODO (KR) : these need to match [[CliApp]]
  type FullEnv = ProvidedEnv & RequiredEnv
  type Effect = RIO[FullEnv, Unit | ExitCode]
  type EnvLayer = RLayer[RequiredEnv, ProvidedEnv]
  type SubApp = CliApp[? <: FullEnv, ?]

  val fullEnvTypeRepr: TypeRepr = TypeRepr.of[FullEnv].simplified
  val effectTypeRepr: TypeRepr = TypeRepr.of[Effect].simplified
  val envLayerTypeRepr: TypeRepr = TypeRepr.of[EnvLayer].simplified
  val subAppTypeRepr: TypeRepr = TypeRepr.of[SubApp].simplified

  private val defaultReg = "^([^$]+)\\$default\\$(\\d+)$".r
  val defaultSyms: Map[(String, Int), Tree] =
    gen.sym.declaredMethods.flatMap { sym =>
      sym.name match
        case defaultReg(name, idx) =>
          val defDef: DefDef = sym.tree.narrow[DefDef]
          val rhs: Term = defDef.rhs.getOrElse { report.errorAndAbort("No default RHS?", sym.pos.getOrElse(gen.pos)) }
          ((name, idx.toInt - 1), rhs).some
        case _ =>
          None
    }.toMap

  report.errorAndAbort(
    defaultSyms.mkString(", "),
  )

}
