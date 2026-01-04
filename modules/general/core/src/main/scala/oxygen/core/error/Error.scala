package oxygen.core.error

import oxygen.core.TypeTag
import oxygen.core.syntax.throwable.*
import oxygen.core.typeclass.Show
import scala.collection.immutable.ArraySeq

trait Error extends Throwable {

  protected[Error] final lazy val javaStackTrace: Error.StackTrace = Error.StackTrace.fromJava(this.getStackTrace)

  def errorType: Error.ErrorType = Error.ErrorType.extract(this)
  def errorMessage: String
  def causes: ArraySeq[Error]
  def traces: ArraySeq[Error.StackTrace] = ArraySeq(javaStackTrace)

  /**
    * @param causeDepth How deep to show nested causes. 0 = no causes, 1 = only immediate children, 2 = up to grandchildren, etc
    * @param traceDepth How deep to show traces.
    * @param typeDepth How deep to include [[errorType]] name in the message.
    */
  final def show(causeDepth: Int, traceDepth: Int, typeDepth: Int): String =
    ??? // FIX-PRE-MERGE (KR) :

  // format: off
  final def showOnlyMessage: String = ??? // FIX-PRE-MERGE (KR) :  
  // format: on

  override final def getMessage: String = errorMessage
  override final def toString: String = errorMessage

}
object Error {

  //////////////////////////////////////////////////////////////////////////////////////////////////////
  //      API
  //////////////////////////////////////////////////////////////////////////////////////////////////////

  def apply(errorMessage: String): Error = fromMessageInternal(errorMessage, ArraySeq.empty)
  def apply(errorMessage: String, cause: Throwable): Error = fromMessageInternal(errorMessage, extractCauses(cause))
  def apply(errorMessage: String, causes: IterableOnce[Throwable]): Error = fromMessageInternal(errorMessage, ArraySeq.from(causes).flatMap(Error.safeWrapThrowable))

  def message(errorMessage: String): Error = fromMessageInternal(errorMessage, ArraySeq.empty)
  def message(errorMessage: String, cause: Throwable): Error = fromMessageInternal(errorMessage, extractCauses(cause))
  def message(errorMessage: String, causes: IterableOnce[Throwable]): Error = fromMessageInternal(errorMessage, ArraySeq.from(causes).flatMap(Error.safeWrapThrowable))

  def show[A: Show](value: A): Error = fromShowMessageInternal(value, ArraySeq.empty)
  def show[A: Show](value: A, cause: Throwable): Error = fromShowMessageInternal(value, extractCauses(cause))
  def show[A: Show](value: A, causes: IterableOnce[Throwable]): Error = fromShowMessageInternal(value, ArraySeq.from(causes).flatMap(Error.safeWrapThrowable))

  def fromThrowable(underlying: Throwable): Error =
    underlying match
      case null         => NullError
      case error: Error => error
      case _            => Error.WrappedThrowable(underlying)

  def fromAny(underlying: Any): Error =
    underlying.asInstanceOf[Matchable] match
      case null             => NullError
      case error: Error     => error
      case error: Throwable => Error.WrappedThrowable(error)
      case _                => Error.WrappedAny(underlying)

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

  enum ErrorType {
    case Tagged(errorTypeTag: TypeTag[?])
    case Named(errorTypeName: String)
    case Unknown

    final def limit(lim: ErrorType.Limit): ErrorType =
      lim match
        case ErrorType.Limit.Tagged  => this
        case ErrorType.Limit.Unknown => ErrorType.Unknown
        case ErrorType.Limit.Named   =>
          this match
            case ErrorType.Tagged(tag) => ErrorType.Named(tag.prefixAllNoGenerics)
            case _                     => this

  }
  object ErrorType {

    enum Limit { case Tagged, Named, Unknown }

    def extract(value: Any): ErrorType =
      if value == null then ErrorType.Unknown
      else ErrorType.Tagged(TypeTag.fromClass(value.getClass))

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
    override lazy val errorType: ErrorType = ErrorType.extract(value)
    override lazy val errorMessage: String = show.show(value)
  }

  final case class WrappedAny private[Error] (value: Any) extends Error.NoStackTrace {
    override lazy val errorType: ErrorType = ErrorType.extract(value)
    override def errorMessage: String = value.toString
    override def causes: ArraySeq[Error] = ArraySeq.empty
    override def traces: ArraySeq[StackTrace] = ArraySeq.empty
  }

  final class WrappedThrowable private[Error] (underlying: Throwable) extends Error.NoStackTrace {
    override lazy val errorType: ErrorType = ErrorType.extract(underlying)
    override lazy val getStackTrace: Array[StackTraceElement] = underlying.getStackTrace
    override lazy val errorMessage: String = underlying.safeGetMessage
    override lazy val causes: ArraySeq[Error] = Error.extractCauses(underlying.getCause)
    override lazy val traces: ArraySeq[Error.StackTrace] = ArraySeq(Error.StackTrace.fromJava(errorMessage.getStackTrace))
  }

  /**
    * final case class MyThing(a: Int, b: String) extends Error.UsingShow[MyThing]
    *
    * If your type has causes in it, and is deriving a [[Show]] instance,
    * it is recommended to use [[Show.annotation.hide]] to remove that field or fields.
    * Then, the showing of said causes are controlled by show [[Error]] show variant you use.
    */
  abstract class UsingShow[A: Show as show] extends Error { self: A =>
    override final def errorMessage: String = show.show(self)
    override def causes: ArraySeq[Error] = ArraySeq.empty // overridable
  }

  final case class Raw(
      override val errorType: ErrorType,
      errorMessage: String,
      causes: ArraySeq[Error.Raw],
      override val traces: ArraySeq[StackTrace],
  ) extends Error.NoStackTrace
  object Raw {

    private def fromInternal(
        error: Error,
        limit: ErrorType.Limit,
        causeDepth: Int,
        traceDepth: Int,
    ): Error.Raw =
      Error.Raw(
        errorType = error.errorType.limit(limit),
        errorMessage = error.errorMessage,
        causes = if causeDepth > 0 then error.causes.map(e => fromInternal(e, limit, causeDepth - 1, traceDepth - 1)) else ArraySeq.empty,
        traces = if traceDepth > 0 then error.traces else ArraySeq.empty,
      )

    def fromError(error: Error, limit: ErrorType.Limit, causeDepth: Int = Int.MaxValue, traceDepth: Int = Int.MaxValue): Error.Raw =
      fromInternal(
        error = error,
        limit = limit,
        causeDepth = causeDepth,
        traceDepth = traceDepth,
      )

    def fromThrowable(throwable: Throwable, limit: ErrorType.Limit, causeDepth: Int = Int.MaxValue, traceDepth: Int = Int.MaxValue): Error.Raw =
      fromInternal(
        error = Error.fromThrowable(throwable),
        limit = limit,
        causeDepth = causeDepth,
        traceDepth = traceDepth,
      )

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
