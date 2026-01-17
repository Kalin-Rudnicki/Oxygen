package oxygen.core.error

import oxygen.core.{str, Text, TypeTag}
import oxygen.core.syntax.option.*
import oxygen.core.syntax.throwable.*
import oxygen.core.typeclass.Show
import oxygen.core.typeclass.Showable
import scala.annotation.tailrec
import scala.collection.immutable.ArraySeq

// TODO (KR) : Have a concept of `cause importance`, an enum along the lines of `NiceToHave`, `Critical`, and potentially things in between.
//           : Then, errors can specify this, and it will guide whether or not to include causes in the default case.
trait Error extends Throwable, Showable {

  protected[Error] final lazy val javaStackTrace: Error.StackTrace = Error.StackTrace.fromJava(this.getStackTrace)

  def errorType: Error.ErrorType = Error.ErrorType.extract(this)
  def errorMessage: Text
  def contextTrace: List[Text] = Nil
  def causes: ArraySeq[Error] = ArraySeq.empty
  def traces: ArraySeq[Error.StackTrace] = ArraySeq(javaStackTrace)

  final def addContext(context: Text.Auto): Error = Error.WithContextTrace.fromError(this, context)

  final def trim(causeDepth: Int = Int.MaxValue, traceDepth: Int = Int.MaxValue, errorType: Error.ErrorType.Limit = Error.ErrorType.Limit.Named): Error.Raw =
    Error.Raw.fromError(this, errorType, causeDepth, traceDepth)

  /**
    * @param causeDepth How deep to show nested causes. 0 = no causes, 1 = only immediate children, 2 = up to grandchildren, etc
    * @param traceDepth How deep to show traces.
    * @param typeDepth How deep to include [[errorType]] name in the message.
    */
  final def show(causeDepth: Int, traceDepth: Int, typeDepth: Int): Text =
    Text.when(typeDepth > 0) { errorType.showHeaderLine } ++
      errorMessage ++
      Text.foreach(contextTrace)(_.indentedInitial("  - ")) ++
      Text.when(traceDepth > 0) { showTraces.indented } ++
      Text.when(causeDepth > 0) { showCauses(causeDepth - 1, traceDepth - 1, typeDepth - 1).indented }

  private final def showTraces: Text = {
    val _traces = traces
    _traces.length match
      case 0 => Text.empty
      case 1 => _traces.head.show { source => str"Stack Trace ($source):" }
      case _ => str"Stack Traces:" ++ Text.foreach(_traces) { _.show { source => str"Stack Trace ($source):" }.indentedInitial("  - ") }
  }

  private final def showCauses(causeDepth: Int, traceDepth: Int, typeDepth: Int): Text = {
    val _causes = causes
    _causes.length match {
      case 0 => Text.empty
      case 1 => Text.fromString("Cause:") ++ _causes.head.show(causeDepth, traceDepth, typeDepth).indented
      case _ =>
        Text.fromString("Causes:") ++
          Text.foreachJoinedWithIndex(_causes, Text.newLine) { (cause, idx) => str"Cause [${idx.toString}]:" ++ cause.show(causeDepth, traceDepth, typeDepth).indented }
    }
  }

  final def showRootMessage: Text = show(0, 0, 0)
  final def showRootSimple: Text = show(0, 0, 1)
  final def showRootTraced: Text = show(0, 1, 1)

  final def showAllMessage: Text = show(Int.MaxValue, 0, 0)
  final def showAllSimple: Text = show(Int.MaxValue, 0, Int.MaxValue)
  final def showAllTraced: Text = show(Int.MaxValue, Int.MaxValue, Int.MaxValue)

  final def showCore(rootTrace: Boolean): Text = show(Int.MaxValue, if rootTrace then 1 else 0, 0)

  override final def show: Text = showCore(false)

  override final def getMessage: String = showCore(false).toString

}
object Error {

  //////////////////////////////////////////////////////////////////////////////////////////////////////
  //      API
  //////////////////////////////////////////////////////////////////////////////////////////////////////

  def apply(errorMessage: Text.Auto): Error = fromMessageInternal(errorMessage, ArraySeq.empty)
  def apply(errorMessage: Text.Auto, cause: Throwable): Error = fromMessageInternal(errorMessage, extractCauses(cause))
  def apply(errorMessage: Text.Auto, causes: IterableOnce[Throwable]): Error = fromMessageInternal(errorMessage, ArraySeq.from(causes).flatMap(Error.safeWrapThrowable))

  def message(errorMessage: Text.Auto): Error = fromMessageInternal(errorMessage, ArraySeq.empty)
  def message(errorMessage: Text.Auto, cause: Throwable): Error = fromMessageInternal(errorMessage, extractCauses(cause))
  def message(errorMessage: Text.Auto, causes: IterableOnce[Throwable]): Error = fromMessageInternal(errorMessage, ArraySeq.from(causes).flatMap(Error.safeWrapThrowable))

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

  final case class StackTrace(
      source: String,
      trace: ArraySeq[Trace],
  ) {

    def show(showHeader: String => Text): Text =
      showHeader(source) ++ Text.foreachJoined(trace, Text.newLine)(_.showLine).indented

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

    final def showHeaderLine: Text = this match
      case ErrorType.Tagged(errorTypeTag) => str"[ ${errorTypeTag.prefixAllNoGenerics} ]:" ++ Text.newLine
      case ErrorType.Named(errorTypeName) => Text.fromString(errorTypeName) ++ Text.newLine
      case ErrorType.Unknown              => Text.empty

  }
  object ErrorType {

    enum Limit { case Tagged, Named, Unknown }

    def extract(value: Any): ErrorType =
      if value == null then ErrorType.Unknown
      else ErrorType.Tagged(TypeTag.fromClass(value.getClass))

  }

  sealed trait Trace {

    final def showLine: Text =
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
    override val errorMessage: Text = Text.fromString("< no error message >")
    override val causes: ArraySeq[Error] = ArraySeq.empty
    override lazy val contextTrace: List[Text] = Nil
    override lazy val traces: ArraySeq[Error.StackTrace] = ArraySeq.empty
  }

  final case class Messageless private[Error] (override val causes: ArraySeq[Error]) extends Error {
    override def errorMessage: Text = Text.fromString("< no error message >")
    override lazy val contextTrace: List[Text] = Nil
  }

  final case class ErrorMessage private[Error] (errorMessage: Text, override val causes: ArraySeq[Error]) extends Error {
    override lazy val contextTrace: List[Text] = Nil
  }

  final case class ShowableErrorMessage[A] private[Error] (value: A, showInst: Show[A], override val causes: ArraySeq[Error]) extends Error {
    override lazy val errorType: ErrorType = ErrorType.extract(value)
    override lazy val errorMessage: Text = Text.fromString(showInst.show(value))
    override lazy val contextTrace: List[Text] = Nil
  }

  final case class WrappedAny private[Error] (value: Any) extends Error.NoStackTrace {
    override lazy val errorType: ErrorType = ErrorType.extract(value)
    override def errorMessage: Text = Text.fromAny(value)
    override lazy val contextTrace: List[Text] = Nil
    override def causes: ArraySeq[Error] = ArraySeq.empty
    override def traces: ArraySeq[StackTrace] = ArraySeq.empty
  }

  final class WrappedThrowable private[Error] (underlying: Throwable) extends Error.NoStackTrace {
    override lazy val errorType: ErrorType = ErrorType.extract(underlying)
    override lazy val getStackTrace: Array[StackTraceElement] = underlying.getStackTrace
    override lazy val errorMessage: Text = Text.fromString(underlying.safeGetMessage)
    override lazy val contextTrace: List[Text] = Nil
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
    override final def errorMessage: Text = Text.fromString(showInst.show(self))
    override def causes: ArraySeq[Error] = ArraySeq.empty // overridable
  }

  final case class WithContextTrace private (underlying: Error, override val contextTrace: List[Text]) extends Error.NoStackTrace {
    override lazy val errorType: ErrorType = underlying.errorType
    override lazy val errorMessage: Text = underlying.errorMessage
    override lazy val traces: ArraySeq[StackTrace] = underlying.traces
    override lazy val causes: ArraySeq[Error] = underlying.causes
  }
  object WithContextTrace {

    def fromError(error: Error, addTrace: Text): WithContextTrace = error match
      case error: WithContextTrace => WithContextTrace(error.underlying, addTrace :: error.contextTrace)
      case _                       => WithContextTrace(error, addTrace :: error.contextTrace)

  }

  final case class Raw(
      override val errorType: ErrorType,
      errorMessage: Text,
      override val contextTrace: List[Text],
      override val causes: ArraySeq[Error.Raw],
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
        contextTrace = error.contextTrace,
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

  private inline def fromMessageInternal(errorMessage: Text.Auto, causes: ArraySeq[Error]): Error =
    if errorMessage == null then
      if causes.isEmpty then Error.NullError
      else Error.ErrorMessage(Text.fromString("< no error message >"), causes)
    else
      Error.ErrorMessage(errorMessage, causes)

  private inline def fromShowMessageInternal[A: Show as show](value: A, causes: ArraySeq[Error]): Error =
    if value == null then
      if causes.isEmpty then Error.NullError
      else Error.Messageless(causes)
    else
      Error.ShowableErrorMessage(value, show, causes)

  final case class EncodedError(
      errorType: Option[String],
      message: String,
      contextTrace: Option[List[String]],
      causes: Option[ArraySeq[EncodedError]],
      traces: Option[Map[String, ArraySeq[EncodedError.Trace]]],
  ) {

    def toError: Error.Raw =
      Error.Raw(
        errorType = errorType match {
          case Some(errorType) => Error.ErrorType.Named(errorType)
          case None            => Error.ErrorType.Unknown
        },
        errorMessage = Text.fromString(message),
        contextTrace = contextTrace.getOrElse(Nil).map(Text.fromString),
        causes = causes.getOrElse(ArraySeq.empty[EncodedError]).map(_.toError),
        traces = ArraySeq.from(traces.getOrElse(Map.empty)).map { case (source, traces) => Error.StackTrace(source, traces.map(_.toErrorTrace)) },
      )

  }
  object EncodedError {

    final case class Trace(
        location: Option[String],
        file: Option[String],
        line: Option[Int],
        raw: Option[String],
    ) {

      def toErrorTrace: Error.Trace = this match
        case Trace(Some(location), Some(file), Some(line), _) => Error.Trace.Rich(location, file, line)
        case Trace(_, _, _, Some(raw))                        => Error.Trace.Raw(raw)
        case _                                                => Error.Trace.Raw("???")

    }
    object Trace {

      @tailrec
      def fromErrorTrace(trace: Error.Trace): Trace =
        trace match
          case trace: Error.Trace.JavaTrace           => Trace.fromErrorTrace(trace.toRich)
          case Error.Trace.Rich(location, file, line) => Trace(location.some, file.some, line.some, None)
          case Error.Trace.Raw(raw)                   => Trace(None, None, None, raw.some)

    }

    def fromError(error: Error): EncodedError = {
      EncodedError(
        errorType = error.errorType match {
          case Error.ErrorType.Tagged(errorTypeTag) => errorTypeTag.prefixAllNoGenerics.some
          case Error.ErrorType.Named(errorTypeName) => errorTypeName.some
          case Error.ErrorType.Unknown              => None
        },
        message = error.errorMessage.toString,
        contextTrace = error.contextTrace.someWhen(_.nonEmpty).map(_.map(_.toString)),
        causes = error.causes.someWhen(_.nonEmpty).map(_.map(EncodedError.fromError)),
        traces = error.traces.someWhen(_.nonEmpty).map { _.map { trace => (trace.source, trace.trace.map(Trace.fromErrorTrace)) }.toMap },
      )
    }

  }

}
