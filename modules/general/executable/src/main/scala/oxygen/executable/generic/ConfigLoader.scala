package oxygen.executable.generic

import oxygen.executable.config.ConfigFileService
import oxygen.json.JsonDecoder
import oxygen.predef.core.*
import oxygen.predef.zio.*
import zio.*

/**
  * Startup-time loader backing `@envConfig`: resolves an env var's value to a file/directory and
  * decodes it into `T`.
  *
  * Delegates to [[ConfigFileService.Live]] so the `@envConfig` startup path and the runtime
  * [[ConfigFileService]] share a single implementation and can never diverge (same file/dir
  * resolution, same JSON/YAML handling, same directory-merge order).
  */
private[executable] object ConfigLoader {

  def loadDecoded[T: JsonDecoder](varName: String, raw: String): IO[String, T] =
    Path.normalizedAbsolute(raw).mapError(_.safeGetMessage)
      .flatMap { path => ConfigFileService.Live.loadResolved[T](path).mapError(_.safeGetMessage) }
      .mapError { error => s"Error extracting environment variable config [$varName]: $error" }

}
