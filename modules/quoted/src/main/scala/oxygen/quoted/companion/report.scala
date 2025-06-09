package oxygen.quoted.companion

import oxygen.quoted.*
import scala.quoted.*

final class reportCompanion(using quotes: Quotes) {

  /** Report an error at the position of the macro expansion */
  def error(msg: String): Unit =
    quotes.reflect.report.error(msg)

  /** Report an error at the position of `expr` */
  def error(msg: String, expr: Expr[Any]): Unit =
    quotes.reflect.report.error(msg, expr)

  /** Report an error message at the given position */
  def error(msg: String, pos: Position): Unit =
    quotes.reflect.report.error(msg, pos.unwrapWithin)

  /** Report an error at the position of the macro expansion and throw a StopMacroExpansion */
  def errorAndAbort(msg: String): Nothing =
    quotes.reflect.report.errorAndAbort(msg)

  /** Report an error at the position of `expr` and throw a StopMacroExpansion */
  def errorAndAbort(msg: String, expr: Expr[Any]): Nothing =
    quotes.reflect.report.errorAndAbort(msg, expr)

  /** Report an error message at the given position and throw a StopMacroExpansion */
  def errorAndAbort(msg: String, pos: Position): Nothing =
    quotes.reflect.report.errorAndAbort(msg, pos.unwrapWithin)

  /** Report a warning at the position of the macro expansion */
  def warning(msg: String): Unit =
    quotes.reflect.report.warning(msg)

  /** Report a warning at the position of `expr` */
  def warning(msg: String, expr: Expr[Any]): Unit =
    quotes.reflect.report.warning(msg, expr)

  /** Report a warning message at the given position */
  def warning(msg: String, pos: Position): Unit =
    quotes.reflect.report.warning(msg, pos.unwrapWithin)

  /** Report an info at the position of the macro expansion */
  def info(msg: String): Unit =
    quotes.reflect.report.info(msg)

  /** Report an info message at the position of `expr` */
  def info(msg: String, expr: Expr[Any]): Unit =
    quotes.reflect.report.info(msg, expr)

  /** Report an info message at the given position */
  def info(msg: String, pos: Position): Unit =
    quotes.reflect.report.info(msg, pos.unwrapWithin)

}
