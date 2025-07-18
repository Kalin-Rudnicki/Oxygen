package oxygen.sql.generic.parsing

import oxygen.predef.core.*
import scala.quoted.*
import scala.util.NotGiven

private[sql] final case class ParseContext private (context: Growable[String]) {
  def :+(ctx: String): ParseContext = ParseContext(context :+ ctx)
  override def toString: String = context.toContiguous.mkString(" -> ")
}
private[sql] object ParseContext {

  def root[A](attempting: String)(f: ParseContext ?=> ParseResult[A])(using NotGiven[ParseContext], Quotes): A =
    f(using ParseContext(Growable.single(attempting))).getOrReport

  def add[A](ctx: String)(f: ParseContext ?=> A)(using parent: ParseContext): A =
    f(using parent :+ ctx)

}
