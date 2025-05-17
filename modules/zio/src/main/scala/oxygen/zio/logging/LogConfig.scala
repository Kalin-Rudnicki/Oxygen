package oxygen.zio.logging

import oxygen.core.ColorMode
import oxygen.core.collection.Contiguous
import oxygen.json.JsonFieldDecoder
import oxygen.predef.json.*
import oxygen.zio.*
import oxygen.zio.instances.given
import oxygen.zio.syntax.log.*
import zio.*
import zio.compat.logOps

final case class LogConfig(
    annotations: Map[String, String],
    spans: List[LogSpan],
    loggers: Contiguous[LogConfig.LoggerElem],
)
object LogConfig {

  final case class Repr(
      annotations: Option[Map[String, String]],
      spans: Option[Map[String, Long]],
      loggers: Json.Obj,
  ) derives JsonDecoder {

    def decode(loggerDecoder: KeyedMapDecoder[LogConfig.LoggerElem]): Either[JsonError, LogConfig] =
      loggerDecoder.decoder.decodeJsonAST(loggers).map { loggers =>
        LogConfig(
          annotations = annotations.getOrElse(Map.empty),
          spans = spans.fold(List.empty[LogSpan])(_.toList.map(LogSpan(_, _))),
          loggers = loggers,
        )
      }

  }

  def oxygenDefault(level: LogLevel): LogConfig = LogConfig(Map.empty, Nil, Contiguous(LoggerElem(Logger.oxygenDefault, level)))
  def oxygenDefault: LogConfig = oxygenDefault(LogLevels.Info)

  final case class LoggerElem(
      logger: Logger,
      level: LogLevel,
  ) {

    def toZLogger: OxygenZLogger =
      OxygenZLogger(logger.logger, level, scala.Console.out)

  }

  private[oxygen] def usingConfig(config: LogConfig): FiberRefModification = {
    logOps.withLoggers(config.loggers.map(_.toZLogger)) >>>
      ZIO.annotated(config.annotations) >>>
      ZIO.withLogSpans(config.spans)
  }

  object elemDecoders {

    private final case class SimpleConfig(
        level: LogLevel,
    ) derives JsonDecoder

    private final case class OxygenConfig(
        level: LogLevel,
        colorMode: Option[ColorMode],
        logTrace: Option[Boolean],
        logFiberId: Option[Boolean],
        logAnnotations: Option[Boolean],
        logSpans: Option[Boolean],
        logTimestamp: Option[Boolean],
        ignoreStackless: Option[Boolean],
    ) derives JsonDecoder

    val zio: KeyedMapDecoder.Decoder[LoggerElem] =
      KeyedMapDecoder.Decoder
        .make[SimpleConfig]("zio")
        .map { c => LoggerElem(Logger.zioDefault, c.level) }

    val raw: KeyedMapDecoder.Decoder[LoggerElem] =
      KeyedMapDecoder.Decoder
        .make[SimpleConfig]("raw-message")
        .map { c => LoggerElem(Logger.rawMessage, c.level) }

    val oxygen: KeyedMapDecoder.Decoder[LoggerElem] =
      KeyedMapDecoder.Decoder
        .make[OxygenConfig]("oxygen")
        .map { c =>
          val logger: Logger =
            Logger.oxygen(
              colorMode = c.colorMode.getOrElse(ColorMode.Extended),
              logTrace = c.logTrace.getOrElse(true),
              logFiberId = c.logFiberId.getOrElse(true),
              logAnnotations = c.logAnnotations.getOrElse(true),
              logSpans = c.logSpans.getOrElse(true),
              logTimestamp = c.logTimestamp.getOrElse(true),
              ignoreStackless = c.ignoreStackless.getOrElse(true),
            )
          LoggerElem(logger, c.level)
        }

    val json: KeyedMapDecoder.Decoder[LoggerElem] =
      KeyedMapDecoder.Decoder
        .make[SimpleConfig]("json")
        .map { c => LoggerElem(Logger.zioDefault, c.level) }

    val default: Contiguous[KeyedMapDecoder.Decoder[LoggerElem]] =
      Contiguous(zio, raw, oxygen, json)

  }

}
