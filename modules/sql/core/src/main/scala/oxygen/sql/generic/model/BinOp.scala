package oxygen.sql.generic.model

import oxygen.predef.color.given
import oxygen.predef.core.*

private[generic] sealed trait BinOp {
  val sql: String
  val scala: String

  def show: String

  final def sqlPadded: String = s" $sql "

}
private[generic] object BinOp {

  enum Comp(final val sql: String, final val scala: String) extends BinOp {
    case `==` extends Comp("=", "==")
    case `!=` extends Comp("!=", "!=")
    case `<` extends Comp("<", "<")
    case `<=` extends Comp("<=", "<=")
    case `>` extends Comp(">", ">")
    case `>=` extends Comp(">=", ">=")

    final def show: String = sql.hexFg("#E6C120").toString

  }

  enum AndOr(final val sql: String, final val scala: String) extends BinOp {
    case `&&` extends AndOr("AND", "&&")
    case `||` extends AndOr("OR", "||")

    final def show: String = sql.hexFg("#FF6666").toString

  }

  // TODO (KR) : combine: `+`. `-`, `*`, `/`, `like`

  val sql: StrictEnum[BinOp] = StrictEnum.derive[BinOp](_.sql)
  val scala: StrictEnum[BinOp] = StrictEnum.derive[BinOp](_.scala)

}
