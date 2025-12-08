package oxygen.http.server

import oxygen.http.model.ServerErrors
import oxygen.json.JsonCodec
import oxygen.predef.core.*
import oxygen.zio.ExtractedCauses

final case class ServerErrorConfig(
    exposeInternalErrors: Boolean,
    includeTraces: Specified[Boolean] = ___,
    includeDefectsOnFailure: Specified[Boolean] = ___,
    includeInterruptsOnFailure: Specified[Boolean] = ___,
    includeInterruptsOnDefect: Specified[Boolean] = ___,
) derives JsonCodec { self =>

  object withDefaults {

    val includeTraces: Boolean = self.includeTraces.getOrElse(self.exposeInternalErrors)
    val includeDefectsOnFailure: Boolean = self.includeDefectsOnFailure.getOrElse(self.exposeInternalErrors)
    val includeInterruptsOnFailure: Boolean = self.includeInterruptsOnFailure.getOrElse(false)
    val includeInterruptsOnDefect: Boolean = self.includeInterruptsOnDefect.getOrElse(false)

  }

  def serverErrors(cause: ExtractedCauses.NoFailures): Option[ServerErrors] =
    Option.when(exposeInternalErrors) {
      ServerErrors.fromCause(
        cause = cause,
        includeTraces = withDefaults.includeTraces,
        includeDefectsOnFailure = withDefaults.includeDefectsOnFailure,
        includeInterruptsOnFailure = withDefaults.includeInterruptsOnFailure,
        includeInterruptsOnDefect = withDefaults.includeInterruptsOnDefect,
      )
    }

}
object ServerErrorConfig {

  // TODO (KR) : RequestDecodingFailureEncoding
  //           : enum RequestDecodingFailureEncoding derives StrictEnum {
  //           :   case UnhandledServerErrorJson
  //           :   case HttpDecodingFailureJson
  //           :   case RequestDecodingFailureJson
  //           : }

}
