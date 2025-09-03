package oxygen.core

import java.util.UUID
import scala.util.Random

trait PlatformCompatPlatformSpecific { self: PlatformCompat =>

  override def randomUUID(): UUID = UUID(Random.nextLong(), Random.nextLong())

}
