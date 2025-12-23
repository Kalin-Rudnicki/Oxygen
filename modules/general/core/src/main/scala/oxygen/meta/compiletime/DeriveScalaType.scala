package oxygen.meta.compiletime

import oxygen.meta.*
import oxygen.quoted.*
import scala.quoted.*

private[compiletime] object DeriveScalaType {

  /*
      It seems a type can be one of a several things:
      1. TypeRef (class=true)
      2.
   */

  def of[T <: AnyKind: Type](using Quotes): ScalaType = {
    val typeRepr0: TypeRepr = TypeRepr.of[T].widen.dealiasKeepOpaques
    lazy val typeSymbol0: Symbol = typeRepr0.typeSymbol
    lazy val typeSymbolCompanion0: Symbol = typeSymbol0.companionModule
    lazy val termSymbol0: Symbol = typeRepr0.termSymbol

    val symbol: Symbol =
      if typeRepr0.isSingleton then termSymbol0
      else if typeSymbol0.fullName.endsWith("$") && !typeSymbolCompanion0.isNoSymbol then typeSymbolCompanion0 else typeSymbol0

    def showSymbol(prefix: String, symbol: Symbol): String = {
      val flags: Flags = symbol.flags
      if symbol.isNoSymbol then
        s"  --- $prefix : <no-symbol> ---"
      else
        s"""  --- $prefix : ${symbol.fullName} ---
           |             Scala2x: ${flags.is(Flags.Scala2x)}
           |                Enum: ${flags.is(Flags.Enum)}
           |                Case: ${flags.is(Flags.Case)}
           |              Sealed: ${flags.is(Flags.Sealed)}
           |               Trait: ${flags.is(Flags.Trait)}
           |    StableRealizable: ${flags.is(Flags.StableRealizable)}
           |              Module: ${flags.is(Flags.Module)}""".stripMargin
    }

    val classInfo: String =
      symbol.tree.narrowOpt[ClassDef] match {
        case Some(classDef) =>
          s"""
             |${classDef.constructor.showAnsiCode}
             |
             |${classDef.constructor.toIndentedString.toStringColorized}
             |""".stripMargin
        case None =>
          "< Not a ClassDef >"
      }

    val infoStr: String =
      s"""
         |=====| TypeRepr |=====
         |  ${typeRepr0.showAnsiCode}
         |  ${typeRepr0.widen.showAnsiCode}
         |  ${typeRepr0.dealias.showAnsiCode}
         |  ${typeRepr0.dealiasKeepOpaques.showAnsiCode}
         |
         |
         |${showSymbol("symbol", symbol)}
         |
         |${showSymbol("typeSymbol", typeSymbol0)}
         |
         |${showSymbol("termSymbol", termSymbol0)}
         |
         |
         |
         |
         |""".stripMargin +
        s"isClass = ${symbol.isClassDef}\n\n${typeRepr0.toIndentedString.toString("|   ")}\n\n$classInfo"

    val path = java.nio.file.Paths.get("target/type-info.txt").toAbsolutePath
    java.nio.file.Files.writeString(path, infoStr)
    report.errorAndAbort(s"wrote to path: $path")

    ??? // FIX-PRE-MERGE (KR) :
  }

}
