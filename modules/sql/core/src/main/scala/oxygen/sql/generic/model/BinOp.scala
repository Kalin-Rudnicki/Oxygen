package oxygen.sql.generic.model

import oxygen.predef.color.given
import oxygen.predef.core.*

private[generic] sealed trait BinOp {
  val sql: String
  val scala: String

  def show: String

  final def sqlPadded: String = s" $sql "

}
private[generic] object BinOp extends Enum.Companion[BinOp] {

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

  override def values: Array[BinOp] = Comp.values ++ AndOr.values

  object Sql extends EnumMap[String](op => NonEmptyList.one(op.sql))
  object Scala extends EnumMap[String](op => NonEmptyList.one(op.scala))

}
