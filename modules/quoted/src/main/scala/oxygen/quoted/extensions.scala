package oxygen.quoted

import scala.quoted.*
import scala.reflect.ClassTag

extension (self: Expr[?])
  def toTerm(using quotes: Quotes): Term =
    Term.wrap(quotes.reflect.asTerm(self))

extension [T <: Tree](self: T)
  def narrow[T2 <: T](using ct: ClassTag[T2]): T2 = self match
    case ct(t2) => t2
    case _      => report.companion(using self.quotes).errorAndAbort(s"Not a '${ct.runtimeClass.getName}':\n${self.unwrap.toString}")
