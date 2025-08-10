package oxygen.quoted

import scala.quoted.*

enum MethodTypeKind {

  /** Represents a parameter list without any implicitness of parameters, like (x1: X1, x2: X2, ...) */
  case Plain

  /** Represents a parameter list with implicit parameters, like `(implicit X1, ..., Xn)`, `(using X1, ..., Xn)`, `(using x1: X1, ..., xn: Xn)` */
  case Implicit

  /** Represents a parameter list of a contextual method, like `(using X1, ..., Xn)` or `(using x1: X1, ..., xn: Xn)` */
  case Contextual

  def unwrapWithin(using newQuotes: Quotes): newQuotes.reflect.MethodTypeKind = this match
    case MethodTypeKind.Plain      => newQuotes.reflect.MethodTypeKind.Plain
    case MethodTypeKind.Implicit   => newQuotes.reflect.MethodTypeKind.Implicit
    case MethodTypeKind.Contextual => newQuotes.reflect.MethodTypeKind.Contextual

}
object MethodTypeKind {

  def wrap(using quotes: Quotes)(unwrap: quotes.reflect.MethodTypeKind): MethodTypeKind = unwrap match
    case quotes.reflect.MethodTypeKind.Plain      => MethodTypeKind.Plain
    case quotes.reflect.MethodTypeKind.Implicit   => MethodTypeKind.Implicit
    case quotes.reflect.MethodTypeKind.Contextual => MethodTypeKind.Contextual

}
