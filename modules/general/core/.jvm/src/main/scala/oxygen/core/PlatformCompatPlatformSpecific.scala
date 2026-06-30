package oxygen.core

import java.util.UUID

trait PlatformCompatPlatformSpecific { self: PlatformCompat =>

  override def randomUUID(): UUID = UUID.randomUUID()

  override def secureRandomBytes(byteCount: Int): Array[Byte] = {
    val bytes = new Array[Byte](byteCount)
    new java.security.SecureRandom().nextBytes(bytes)
    bytes
  }

  override def typeNameAndArgs(klass: Class[?]): (String, List[String]) =
    (
      klass.getName,
      klass.getTypeParameters.toList.map(_.getName),
    )

}
