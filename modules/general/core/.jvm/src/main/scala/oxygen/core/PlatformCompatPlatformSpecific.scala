package oxygen.core

import java.util.UUID

trait PlatformCompatPlatformSpecific { self: PlatformCompat =>

  override def randomUUID(): UUID = UUID.randomUUID()

  override def typeNameAndArgs(klass: Class[?]): (String, List[String]) =
    (
      klass.getName,
      klass.getTypeParameters.toList.map(_.getName),
    )

}
