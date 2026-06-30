package oxygen.core

import java.util.UUID

trait PlatformCompatPlatformSpecific { self: PlatformCompat =>

  override def randomUUID(): UUID = UUID.randomUUID()

  // Native is not an OIDC target; fall back to non-secure Random just so this compiles (cf. randomUUID).
  override def secureRandomBytes(byteCount: Int): Array[Byte] = {
    val bytes = new Array[Byte](byteCount)
    scala.util.Random.nextBytes(bytes)
    bytes
  }

  override def typeNameAndArgs(klass: Class[?]): (String, List[String]) =
    (
      klass.getName,
      klass.getTypeParameters.toList.map(_.getName),
    )

}
