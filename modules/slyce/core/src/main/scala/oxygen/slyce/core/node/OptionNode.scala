package oxygen.slyce.core.node

import oxygen.slyce.core.*

sealed trait OptionNode[+A] extends Node {

  final def toOption: Option[A] = this match
    case OptionNode.SomeNode(value) => Some(value)
    case OptionNode.NoneNode()      => None

}
object OptionNode {

  final case class SomeNode[+A] private[OptionNode] (value: A) extends OptionNode[A]
  final case class NoneNode private[OptionNode] () extends OptionNode[Nothing]

  def some[A](value: A, span: Span.HasPosition): OptionNode[A] = {
    val node = SomeNode(value)
    node.initializeNode(span)
    node
  }

  def some[A <: Element](value: A): OptionNode[A] =
    OptionNode.some(value, value.span)

  def none(span: Span.ZeroWidth): OptionNode[Nothing] = {
    val node = NoneNode()
    node.initializeNode(span)
    node
  }

}
