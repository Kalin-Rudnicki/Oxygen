package oxygen.slyce.core

import oxygen.predef.core.*

sealed trait Element {

  // this prevents something extending Token and Node
  val elementType: String

  def span: Span.HasPosition

  // TODO (KR) :
  // val elementName: String

}

trait Token extends Element {

  override final val elementType: String = "Token"

  override final def span: Span.Range = {
    enforceIsInitialized()
    spanRef
  }

  final def text: String = {
    enforceIsInitialized()
    textRef
  }

  //////////////////////////////////////////////////////////////////////////////////////////////////////
  //      Mutable Nonsense
  //////////////////////////////////////////////////////////////////////////////////////////////////////

  private var isInitialized: Boolean = false
  private var spanRef: Span.Range = null
  private var textRef: String = null

  private def enforceIsInitialized(): Unit =
    if (!isInitialized)
      throw new RuntimeException(s"Token was created without initialization: $this")

  private def enforceIsNotInitialized(): Unit =
    if (isInitialized)
      throw new RuntimeException(s"Token was already initialized: $this")

  private[core] final def initializeToken(span: Span.Range, text: String): Unit = {
    enforceIsNotInitialized()
    isInitialized = true
    spanRef = span
    textRef = text
  }

}
object Token {

  trait Builder[A <: Token] {
    def attemptBuild(text: String): Either[String, A]
  }
  object Builder {

    trait Infallible[A <: Token] extends Builder[A] {
      def build(text: String): A
      override final def attemptBuild(text: String): Either[String, A] = build(text).asRight
    }

    def noParams[A <: Token](f: => A): Builder.Infallible[A] = Builder.NoParams { () => f }
    def withString[A <: Token](f: String => A): Builder.Infallible[A] = Builder.WithString(f)

    final case class NoParams[A <: Token](f: () => A) extends Builder.Infallible[A] {
      override def build(text: String): A = f()
    }

    final case class WithString[A <: Token](f: String => A) extends Builder.Infallible[A] {
      override def build(text: String): A = f(text)
    }

  }

}

trait Node extends Element {

  override final val elementType: String = "Node"

  override final def span: Span.HasPosition = {
    enforceIsInitialized()
    spanRef
  }

  //////////////////////////////////////////////////////////////////////////////////////////////////////
  //      Mutable Nonsense
  //////////////////////////////////////////////////////////////////////////////////////////////////////

  private var isInitialized: Boolean = false
  private var spanRef: Span.HasPosition = null

  private def enforceIsInitialized(): Unit =
    if (!isInitialized)
      throw new RuntimeException(s"Node was created without initialization: $this")

  private def enforceIsNotInitialized(): Unit =
    if (isInitialized)
      throw new RuntimeException(s"Node was already initialized: $this")

  private[core] final def initializeNode(span: Span.HasPosition): Unit = {
    enforceIsNotInitialized()
    isInitialized = true
    spanRef = span
  }

}
