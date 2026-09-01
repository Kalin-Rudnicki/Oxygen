package oxygen.mcp.api.model.request

import oxygen.predef.core.*

/** Syslog-style log severities (RFC 5424). Deprecated as of 2026-07-28 (SEP-2577). */
enum LoggingLevel(final val value: String) derives StrictEnum {
  case Debug extends LoggingLevel("debug")
  case Info extends LoggingLevel("info")
  case Notice extends LoggingLevel("notice")
  case Warning extends LoggingLevel("warning")
  case Error extends LoggingLevel("error")
  case Critical extends LoggingLevel("critical")
  case Alert extends LoggingLevel("alert")
  case Emergency extends LoggingLevel("emergency")

  override final def toString: String = value
}
