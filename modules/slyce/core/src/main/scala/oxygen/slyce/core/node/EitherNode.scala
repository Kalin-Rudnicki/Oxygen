package oxygen.slyce.core.node

import oxygen.slyce.core.*

sealed trait EitherNode[+A, +B] extends Node {

  final def toEither: Either[A, B] = this match
    case EitherNode.RightNode(value) => Right(value)
    case EitherNode.LeftNode(value)  => Left(value)

}
object EitherNode {

  final case class RightNode[+B] private[EitherNode] (value: B) extends EitherNode[Nothing, B]
  final case class LeftNode[+A] private[EitherNode] (value: A) extends EitherNode[A, Nothing]

  def left[A](value: A, span: Span.HasPosition): EitherNode[A, Nothing] = {
    val node = LeftNode(value)
    node.initializeNode(span)
    node
  }

  def left[A <: Element](value: A): EitherNode[A, Nothing] =
    EitherNode.left(value, value.span)

  def right[A](value: A, span: Span.HasPosition): EitherNode[Nothing, A] = {
    val node = RightNode(value)
    node.initializeNode(span)
    node
  }

  def right[B <: Element](value: B): EitherNode[Nothing, B] =
    EitherNode.right(value, value.span)

}
