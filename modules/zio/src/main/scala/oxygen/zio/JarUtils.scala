package oxygen.zio

import java.io.InputStream
import oxygen.zio.error.JarResourceError
import zio.*

object JarUtils {

  private def readStream(path: String, stream: InputStream): IO[JarResourceError.Generic, String] =
    ZIO.attempt { new String(stream.readAllBytes()) }.mapError(JarResourceError.Generic(path, _))

  def findInputStream(path: String): ZIO[Scope, JarResourceError.Generic, Option[InputStream]] =
    ZIO
      .attempt { Option(this.getClass.getClassLoader.getResourceAsStream(path)) }
      .mapError(JarResourceError.Generic(path, _))
      .withFinalizer(ZIO.foreachDiscard(_) { stream => ZIO.attempt { stream.close() }.orDie })

  def findString(path: String): IO[JarResourceError.Generic, Option[String]] =
    ZIO.scoped { findInputStream(path).some.flatMap(readStream(path, _).asSomeError).unsome }

  def getInputStream(path: String): ZIO[Scope, JarResourceError, InputStream] =
    findInputStream(path).someOrFail(JarResourceError.PathDNE(path))

  def getString(path: String): IO[JarResourceError, String] =
    ZIO.scoped { getInputStream(path).flatMap(readStream(path, _)) }

}
