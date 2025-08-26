package oxygen.core

import java.util.UUID

trait PlatformCompatPlatformSpecific { self: PlatformCompat =>

  override def randomUUID(): UUID = UUID.randomUUID()

}
