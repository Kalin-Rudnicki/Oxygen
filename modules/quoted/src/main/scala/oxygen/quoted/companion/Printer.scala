package oxygen.quoted.companion

import oxygen.quoted.*
import scala.quoted.*

final class PrinterCompanion(using quotes: Quotes) {

  extension [Raw](self: quotes.reflect.Printer[Raw])
    private def wrapped[A](convert: A => Raw): Printer[A] =
      a => self.show(convert(a))

  /** Prints fully elaborated version of the source code. */
  def TreeCode: Printer[Tree] = quotes.reflect.Printer.TreeCode.wrapped(_.unwrapWithin)

  /**
    * Prints fully elaborated version of the source code.
    *  Same as `TreeCode` but does not print full package prefixes.
    */
  def TreeShortCode: Printer[Tree] = quotes.reflect.Printer.TreeShortCode.wrapped(_.unwrapWithin)

  /** Prints fully elaborated version of the source code using ANSI colors. */
  def TreeAnsiCode: Printer[Tree] = quotes.reflect.Printer.TreeAnsiCode.wrapped(_.unwrapWithin)

  /**
    * Prints a pattern like representation of the `Tree`.
    *  It displays the structure of the AST.
    */
  def TreeStructure: Printer[Tree] = quotes.reflect.Printer.TreeStructure.wrapped(_.unwrapWithin)

  /** Prints the type in source code. */
  def TypeReprCode: Printer[TypeRepr] = quotes.reflect.Printer.TypeReprCode.wrapped(_.unwrapWithin)

  /**
    * Prints the type in source code.
    *  Same as `TypeReprCode` but does not print full package prefixes.
    */
  def TypeReprShortCode: Printer[TypeRepr] = quotes.reflect.Printer.TypeReprShortCode.wrapped(_.unwrapWithin)

  /** Prints the type in source code using ANSI colors. */
  def TypeReprAnsiCode: Printer[TypeRepr] = quotes.reflect.Printer.TypeReprAnsiCode.wrapped(_.unwrapWithin)

  /**
    * Prints a pattern like representation of the `TypeRepr`.
    *  It displays the structure of the type.
    */
  def TypeReprStructure: Printer[TypeRepr] = quotes.reflect.Printer.TypeReprStructure.wrapped(_.unwrapWithin)

  /** Prints the constant in source code. */
  def ConstantCode: Printer[Constant] = quotes.reflect.Printer.ConstantCode.wrapped(_.unwrapWithin)

  /** Prints a pattern like representation of the `Constant`. */
  def ConstantStructure: Printer[Constant] = quotes.reflect.Printer.ConstantStructure.wrapped(_.unwrapWithin)

}
