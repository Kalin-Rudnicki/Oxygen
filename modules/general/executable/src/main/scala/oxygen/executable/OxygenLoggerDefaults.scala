package oxygen.executable

import oxygen.core.ColorMode

final case class OxygenLoggerDefaults(
    colorMode: ColorMode,
    logTrace: Boolean,
    logFiberId: Boolean,
    logAnnotations: Boolean,
    logSpans: Boolean,
    logTimestamp: Boolean,
    ignoreStackless: Boolean,
)
object OxygenLoggerDefaults {

  def all(
      colorMode: ColorMode = ColorMode.Extended,
      logTrace: Boolean = true,
      logFiberId: Boolean = true,
      logAnnotations: Boolean = true,
      logSpans: Boolean = true,
      logTimestamp: Boolean = true,
      ignoreStackless: Boolean = true,
  ): OxygenLoggerDefaults =
    OxygenLoggerDefaults(
      colorMode = colorMode,
      logTrace = logTrace,
      logFiberId = logFiberId,
      logAnnotations = logAnnotations,
      logSpans = logSpans,
      logTimestamp = logTimestamp,
      ignoreStackless = ignoreStackless,
    )

  def lean(
      colorMode: ColorMode = ColorMode.Extended,
      logTrace: Boolean = false,
      logFiberId: Boolean = false,
      logAnnotations: Boolean = true,
      logSpans: Boolean = true,
      logTimestamp: Boolean = false,
      ignoreStackless: Boolean = false,
  ): OxygenLoggerDefaults =
    OxygenLoggerDefaults(
      colorMode = colorMode,
      logTrace = logTrace,
      logFiberId = logFiberId,
      logAnnotations = logAnnotations,
      logSpans = logSpans,
      logTimestamp = logTimestamp,
      ignoreStackless = ignoreStackless,
    )

  def none(
      colorMode: ColorMode = ColorMode.Extended,
      logTrace: Boolean = false,
      logFiberId: Boolean = false,
      logAnnotations: Boolean = false,
      logSpans: Boolean = false,
      logTimestamp: Boolean = false,
      ignoreStackless: Boolean = false,
  ): OxygenLoggerDefaults =
    OxygenLoggerDefaults(
      colorMode = colorMode,
      logTrace = logTrace,
      logFiberId = logFiberId,
      logAnnotations = logAnnotations,
      logSpans = logSpans,
      logTimestamp = logTimestamp,
      ignoreStackless = ignoreStackless,
    )

}
