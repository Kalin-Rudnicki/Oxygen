package oxygen.core

import java.util.UUID

trait PlatformCompat {

  def randomUUID(): UUID

  /** Cryptographically-strong random bytes (JVM `SecureRandom` / browser `crypto.getRandomValues`). */
  def secureRandomBytes(byteCount: Int): Array[Byte]

  def typeNameAndArgs(klass: Class[?]): (String, List[String])

}
object PlatformCompat extends PlatformCompat, PlatformCompatPlatformSpecific
