package oxygen.quoted

import oxygen.quoted.companion.PrinterCompanion
import scala.quoted.*
import scala.reflect.ClassTag

extension (self: Expr[?]) {

  def toTerm(using quotes: Quotes): Term =
    Term.wrap(quotes.reflect.asTerm(self))

  def showWith(f: PrinterCompanion => Printer[Tree])(using Quotes): String = self.toTerm.showWith(f)
  def showCode(using Quotes): String = self.toTerm.showCode
  def showShortCode(using Quotes): String = self.toTerm.showShortCode
  def showAnsiCode(using Quotes): String = self.toTerm.showAnsiCode
  def showStructure(using Quotes): String = self.toTerm.showStructure

}

extension (self: Type[?]) {

  def toTypeRepr(using quotes: Quotes): TypeRepr =
    TypeRepr.fromType(self)

}

extension [T <: Model](self: T) {

  def narrow[T2 <: T](hint: String)(using ct: ClassTag[T2]): T2 = self match
    case ct(t2) => t2
    case _      => report.companion(using self.quotes).errorAndAbort(s"Not a '${ct.runtimeClass.getName}': ${self.getClass.getName}\n($hint)\n\n$self", self.maybePos)

  def narrow[T2 <: T](using ct: ClassTag[T2]): T2 = self match
    case ct(t2) => t2
    case _      => report.companion(using self.quotes).errorAndAbort(s"Not a '${ct.runtimeClass.getName}': ${self.getClass.getName}\n\n$self", self.maybePos)

  def narrowOpt[T2 <: T](using ct: ClassTag[T2]): Option[T2] =
    ct.unapply(self)

}
