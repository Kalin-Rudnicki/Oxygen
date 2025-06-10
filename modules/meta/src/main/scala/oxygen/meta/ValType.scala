package oxygen.meta

import oxygen.quoted.*
import scala.quoted.*

enum ValType {
  case Val, LazyVal, Var

  def toFlags(using Quotes): Flags = this match
    case ValType.Val     => Flags.EmptyFlags
    case ValType.LazyVal => Flags.Lazy
    case ValType.Var     => Flags.Mutable

}
