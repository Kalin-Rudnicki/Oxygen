package oxygen.core.error

import oxygen.core.{str, LazyString, TypeTag}
import oxygen.core.syntax.throwable.*
import oxygen.core.typeclass.Show
import oxygen.core.typeclass.Showable
import scala.collection.immutable.ArraySeq

trait Error extends Throwable, Showable {

  protected[Error] final lazy val javaStackTrace: Error.StackTrace = Error.StackTrace.fromJava(this.getStackTrace)

  def errorType: Error.ErrorType = Error.ErrorType.extract(this)
  def errorMessage: LazyString
  def causes: ArraySeq[Error]
  def traces: ArraySeq[Error.StackTrace] = ArraySeq(javaStackTrace)

  final def trim(causeDepth: Int = Int.MaxValue, traceDepth: Int = Int.MaxValue, errorType: Error.ErrorType.Limit = Error.ErrorType.Limit.Named): Error.Raw =
    Error.Raw.fromError(this, errorType, causeDepth, traceDepth)

  /**
    * @param causeDepth How deep to show nested causes. 0 = no causes, 1 = only immediate children, 2 = up to grandchildren, etc
    * @param traceDepth How deep to show traces.
    * @param typeDepth How deep to include [[errorType]] name in the message.
    */
  final def show(causeDepth: Int, traceDepth: Int, typeDepth: Int): LazyString =
    LazyString.when(typeDepth > 0) { errorType.showHeaderLine } ++
      errorMessage ++
      LazyString.when(traceDepth > 0) { showTraces.indented } ++
      LazyString.when(causeDepth > 0) { showCauses(causeDepth - 1, traceDepth - 1, typeDepth - 1).indented }

  private def showTraces: LazyString = {
    val _traces = traces
    _traces.length match
      case 0 => LazyString.empty
      case 1 => _traces.head.show { source => str"Stack Trace ($source):" }
      case _ => str"Stack Traces:" ++ LazyString.foreach(_traces) { _.show { source => str"Stack Trace ($source):" }.indentedInitial("  - ") }
  }

  private def showCauses(causeDepth: Int, traceDepth: Int, typeDepth: Int): LazyString = {
    val _causes = causes
    _causes.length match {
      case 0 => LazyString.empty
      case 1 => LazyString.fromString("Cause:") ++ _causes.head.show(causeDepth, traceDepth, typeDepth).indented
      case _ =>
        LazyString.fromString("Causes:") ++
          LazyString.foreachJoinedWithIndex(_causes, LazyString.newLine) { (cause, idx) => str"Cause [${idx.toString}]:" ++ cause.show(causeDepth, traceDepth, typeDepth).indented }
    }
  }

  final def showRootMessage: LazyString = show(0, 0, 0)
  final def showRootSimple: LazyString = show(0, 0, 1)
  final def showRootTraced: LazyString = show(0, 1, 1)

  final def showAllMessage: LazyString = show(Int.MaxValue, 0, 0)
  final def showAllSimple: LazyString = show(Int.MaxValue, 0, Int.MaxValue)
  final def showAllTraced: LazyString = show(Int.MaxValue, Int.MaxValue, Int.MaxValue)

  final def showCore(rootTrace: Boolean): LazyString = show(Int.MaxValue, if rootTrace then 1 else 0, 0)

  override final def show: LazyString = showCore(true)

  override final def getMessage: String = showCore(false).toString

}
object Error {

  //////////////////////////////////////////////////////////////////////////////////////////////////////
  //      API
  //////////////////////////////////////////////////////////////////////////////////////////////////////

  def apply(errorMessage: LazyString.Auto): Error = fromMessageInternal(errorMessage, ArraySeq.empty)
  def apply(errorMessage: LazyString.Auto, cause: Throwable): Error = fromMessageInternal(errorMessage, extractCauses(cause))
  def apply(errorMessage: LazyString.Auto, causes: IterableOnce[Throwable]): Error = fromMessageInternal(errorMessage, ArraySeq.from(causes).flatMap(Error.safeWrapThrowable))

  def message(errorMessage: LazyString.Auto): Error = fromMessageInternal(errorMessage, ArraySeq.empty)
  def message(errorMessage: LazyString.Auto, cause: Throwable): Error = fromMessageInternal(errorMessage, extractCauses(cause))
  def message(errorMessage: LazyString.Auto, causes: IterableOnce[Throwable]): Error = fromMessageInternal(errorMessage, ArraySeq.from(causes).flatMap(Error.safeWrapThrowable))

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

  // TODO (KR) : consider if we really want this...
  // given stringToError: Conversion[String, Error] = Error(_)

  //////////////////////////////////////////////////////////////////////////////////////////////////////
  //      Models
  //////////////////////////////////////////////////////////////////////////////////////////////////////

  final case class StackTrace private[error] (
      source: String,
      trace: ArraySeq[Trace],
  ) {

    def show(showHeader: String => LazyString): LazyString =
      showHeader(source) ++ LazyString.foreachJoined(trace, LazyString.newLine)(_.showLine).indented

  }
  object StackTrace {

    private val ignoredPrefixes: IArray[String] =
      IArray(
        "oxygen.core.error.",
        "oxygen.zio.",
        "zio.internal.",
        "zio.ZIO",
      )

    def fromJava(traces: Array[StackTraceElement]): StackTrace =
      StackTrace(
        "Java",
        if traces == null then ArraySeq.empty[Trace]
        else
          ArraySeq
            .unsafeWrapArray(traces)
            .filter { trace =>
              (trace ne null) && !ignoredPrefixes.exists(trace.getClassName.startsWith)
            }
            .map { trace => Trace.JavaTrace(trace) },
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

    final def showHeaderLine: LazyString = this match
      case ErrorType.Tagged(errorTypeTag) => str"[ ${errorTypeTag.prefixAllNoGenerics} ]:" ++ LazyString.newLine
      case ErrorType.Named(errorTypeName) => LazyString.fromString(errorTypeName) ++ LazyString.newLine
      case ErrorType.Unknown              => LazyString.empty

  }
  object ErrorType {

    enum Limit { case Tagged, Named, Unknown }

    def extract(value: Any): ErrorType =
      if value == null then ErrorType.Unknown
      else ErrorType.Tagged(TypeTag.fromClass(value.getClass))

  }

  sealed trait Trace {

    final def showLine: LazyString =
      this match {
        case Trace.JavaTrace(element) => str"- ${element.getClassName.stripSuffix("$")}<${element.getMethodName.takeWhile(_ != '$')}> : ${element.getFileName} : ${element.getLineNumber.toString}"
        case Trace.Rich(location, file, line) => str"- $location : $file : ${line.toString}"
        case Trace.Raw(raw)                   => str"- $raw"
      }

  }
  object Trace {

    final case class JavaTrace(element: StackTraceElement) extends Trace {
      def toRich: Trace.Rich = Trace.Rich(str"${element.getClassName.stripSuffix("$")}<${element.getMethodName.takeWhile(_ != '$')}>".toString, element.getFileName, element.getLineNumber)
    }

    final case class Rich(location: String, file: String, line: Int) extends Trace
    final case class Raw(raw: String) extends Trace

  }

  abstract class NoStackTrace extends Throwable(null, null, false, true), Error

  case object NullError extends Error.NoStackTrace {
    override val errorMessage: LazyString = LazyString.fromString("< no error message >")
    override val causes: ArraySeq[Error] = ArraySeq.empty
    override lazy val traces: ArraySeq[Error.StackTrace] = ArraySeq.empty
  }

  final case class Messageless private[Error] (causes: ArraySeq[Error]) extends Error {
    override def errorMessage: LazyString = LazyString.fromString("< no error message >")
  }

  final case class ErrorMessage private[Error] (errorMessage: LazyString, causes: ArraySeq[Error]) extends Error

  final case class ShowableErrorMessage[A] private[Error] (value: A, showInst: Show[A], causes: ArraySeq[Error]) extends Error {
    override lazy val errorType: ErrorType = ErrorType.extract(value)
    override lazy val errorMessage: LazyString = LazyString.fromString(showInst.show(value))
  }

  final case class WrappedAny private[Error] (value: Any) extends Error.NoStackTrace {
    override lazy val errorType: ErrorType = ErrorType.extract(value)
    override def errorMessage: LazyString = LazyString.fromAny(value)
    override def causes: ArraySeq[Error] = ArraySeq.empty
    override def traces: ArraySeq[StackTrace] = ArraySeq.empty
  }

  final class WrappedThrowable private[Error] (underlying: Throwable) extends Error.NoStackTrace {
    override lazy val errorType: ErrorType = ErrorType.extract(underlying)
    override lazy val getStackTrace: Array[StackTraceElement] = underlying.getStackTrace
    override lazy val errorMessage: LazyString = LazyString.fromString(underlying.safeGetMessage)
    override lazy val causes: ArraySeq[Error] = Error.extractCauses(underlying.getCause)
    override lazy val traces: ArraySeq[Error.StackTrace] = ArraySeq(Error.StackTrace.fromJava(underlying.getStackTrace))
  }

  /**
    * final case class MyThing(a: Int, b: String) extends Error.UsingShow[MyThing]
    *
    * If your type has causes in it, and is deriving a [[Show]] instance,
    * it is recommended to use [[Show.annotation.hide]] to remove that field or fields.
    * Then, the showing of said causes are controlled by show [[Error]] show variant you use.
    */
  abstract class UsingShow[A: Show as showInst] extends Error { self: A =>
    override final def errorMessage: LazyString = LazyString.fromString(showInst.show(self))
    override def causes: ArraySeq[Error] = ArraySeq.empty // overridable
  }

  final case class Raw(
      override val errorType: ErrorType,
      errorMessage: LazyString,
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

  private inline def fromMessageInternal(errorMessage: LazyString.Auto, causes: ArraySeq[Error]): Error =
    if errorMessage == null then
      if causes.isEmpty then Error.NullError
      else Error.ErrorMessage(LazyString.fromString("< no error message >"), causes)
    else
      Error.ErrorMessage(errorMessage, causes)

  private inline def fromShowMessageInternal[A: Show as show](value: A, causes: ArraySeq[Error]): Error =
    if value == null then
      if causes.isEmpty then Error.NullError
      else Error.Messageless(causes)
    else
      Error.ShowableErrorMessage(value, show, causes)

}
