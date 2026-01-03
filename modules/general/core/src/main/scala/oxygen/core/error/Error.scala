package oxygen.core.error

import oxygen.core.TypeTag
import oxygen.core.syntax.throwable.*
import oxygen.core.typeclass.Show
import scala.collection.immutable.ArraySeq

trait Error extends Throwable {

  protected[Error] final lazy val javaStackTrace: Error.StackTrace = Error.StackTrace.fromJava(this.getStackTrace)

  def tag: TypeTag[?] = TypeTag.fromClass(this.getClass)

  def errorMessage: String
  def causes: ArraySeq[Error]
  def traces: ArraySeq[Error.StackTrace] = ArraySeq(javaStackTrace)

  override final def getMessage: String = errorMessage
  override final def toString: String = errorMessage

}
object Error {

  //////////////////////////////////////////////////////////////////////////////////////////////////////
  //      API
  //////////////////////////////////////////////////////////////////////////////////////////////////////

  def apply(errorMessage: String): Error = if errorMessage == null then Error.NullError else fromMessageInternal(errorMessage, ArraySeq.empty)
  def apply(errorMessage: String, cause: Throwable): Error = fromMessageInternal(errorMessage, extractCauses(cause))
  def apply(errorMessage: String, causes: IterableOnce[Throwable]): Error = fromMessageInternal(errorMessage, ArraySeq.from(causes).flatMap(Error.safeWrapThrowable))

  def message(errorMessage: String): Error = if errorMessage == null then Error.NullError else fromMessageInternal(errorMessage, ArraySeq.empty)
  def message(errorMessage: String, cause: Throwable): Error = fromMessageInternal(errorMessage, extractCauses(cause))
  def message(errorMessage: String, causes: IterableOnce[Throwable]): Error = fromMessageInternal(errorMessage, ArraySeq.from(causes).flatMap(Error.safeWrapThrowable))

  def wrap(underlying: Throwable): Error =
    underlying match
      case null         => NullError
      case error: Error => error
      case _            => Error.WrappedThrowable(underlying)

  given stringToError: Conversion[String, Error] = Error(_)

  //////////////////////////////////////////////////////////////////////////////////////////////////////
  //      Models
  //////////////////////////////////////////////////////////////////////////////////////////////////////

  final case class StackTrace private[error] (
      source: String,
      trace: ArraySeq[Trace],
  )
  object StackTrace {

    def fromJava(traces: Array[StackTraceElement]): StackTrace =
      StackTrace(
        "Java",
        if traces == null then ArraySeq.empty[Trace]
        else
          ArraySeq.unsafeWrapArray(traces).map {
            case null  => Trace.Raw("")
            case trace => Trace.JavaTrace(trace)
          },
      )

    def raw(source: String, traces: IterableOnce[String]): StackTrace =
      StackTrace(
        if source == null then "Unknown" else source,
        if traces == null then ArraySeq.empty[Trace]
        else
          ArraySeq.from(traces).map {
            case null  => Trace.Raw("")
            case trace => Trace.Raw(trace)
          },
      )

  }

  sealed trait Trace
  object Trace {
    final case class JavaTrace(element: StackTraceElement) extends Trace
    final case class Rich(location: String, file: String, line: Int) extends Trace
    final case class Raw(raw: String) extends Trace
  }

  abstract class NoStackTrace extends Throwable(null, null, false, true), Error

  case object NullError extends Error.NoStackTrace {
    override val errorMessage: String = "< no error message >"
    override val causes: ArraySeq[Error] = ArraySeq.empty
    override lazy val traces: ArraySeq[Error.StackTrace] = ArraySeq.empty
  }

  final case class Messageless private[Error] (causes: ArraySeq[Error]) extends Error {
    override def errorMessage: String = "< no error message >"
  }

  final case class ErrorMessage private[Error] (errorMessage: String, causes: ArraySeq[Error]) extends Error

  final case class ShowableErrorMessage[A] private[Error] (value: A, show: Show[A], causes: ArraySeq[Error]) extends Error {
    override lazy val errorMessage: String = show.show(value)
  }

  final class WrappedThrowable private[Error] (underlying: Throwable) extends Error.NoStackTrace {
    override def tag: TypeTag[?] = TypeTag.fromClass(underlying.getClass)
    override lazy val getStackTrace: Array[StackTraceElement] = underlying.getStackTrace
    override lazy val errorMessage: String = underlying.safeGetMessage
    override lazy val causes: ArraySeq[Error] = Error.extractCauses(underlying.getCause)
    override lazy val traces: ArraySeq[Error.StackTrace] = ArraySeq(Error.StackTrace.fromJava(errorMessage.getStackTrace))
  }

  final case class Raw(
      typeName: Option[String],
      typeTag: Option[TypeTag[?]],
      errorMessage: String,
      causes: ArraySeq[Error.Raw],
      override val traces: ArraySeq[StackTrace],
  ) extends Error.NoStackTrace
  object Raw {

    private def fromInternal(
        error: Error,
        tt: => TypeTag[?],
        typeName: Boolean,
        typeTag: Boolean,
        causes: Boolean,
        traces: Boolean,
    ): Error.Raw =
      error match {
        case encoded: Error.Raw => encoded
        case _                  =>
          lazy val tag: TypeTag[?] = tt
          Error.Raw(
            typeName = Option.when(typeName)(tag.prefixAllNoGenerics),
            typeTag = Option.when(typeTag)(tag),
            errorMessage = error.errorMessage,
            causes = if causes then error.causes.map(e => fromInternal(e, e.tag, typeName, typeTag, causes, traces)) else ArraySeq.empty,
            traces = if traces then error.traces else ArraySeq.empty,
          )
      }

    def fromError(error: Error, typeName: Boolean = true, typeTag: Boolean = false, causes: Boolean = true, traces: Boolean = true): Error.Raw =
      fromInternal(
        error = error,
        tt = error.tag,
        typeName = typeName,
        typeTag = typeTag,
        causes = causes,
        traces = traces,
      )

    def fromThrowable(throwable: Throwable, typeName: Boolean = true, typeTag: Boolean = false, causes: Boolean = true, traces: Boolean = true): Error.Raw = {
      val error = Error.wrap(throwable)
      fromInternal(
        error = error,
        tt = error.tag,
        typeName = typeName,
        typeTag = typeTag,
        causes = causes,
        traces = traces,
      )
    }

  }

  //////////////////////////////////////////////////////////////////////////////////////////////////////
  //      Helpers
  //////////////////////////////////////////////////////////////////////////////////////////////////////

  private def safeWrapThrowable(underlying: Throwable): Option[Error] =
    underlying match
      case null         => None
      case error: Error => Some(error)
      case _            => Some(Error.WrappedThrowable(underlying))

  private def extractCauses(cause: Throwable): ArraySeq[Error] =
    safeWrapThrowable(cause) match
      case Some(value) => ArraySeq(value)
      case None        => ArraySeq.empty

  private inline def fromMessageInternal(errorMessage: String, causes: ArraySeq[Error]): Error =
    if errorMessage == null then
      if causes.isEmpty then Error.NullError
      else Error.ErrorMessage("< no error message >", causes)
    else
      Error.ErrorMessage(errorMessage, causes)

  private inline def fromShowMessageInternal[A: Show as show](value: A, causes: ArraySeq[Error]): Error =
    if value == null then
      if causes.isEmpty then Error.NullError
      else Error.Messageless(causes)
    else
      Error.ShowableErrorMessage(value, show, causes)

}
