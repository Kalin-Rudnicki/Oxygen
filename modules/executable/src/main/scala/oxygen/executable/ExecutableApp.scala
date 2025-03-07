package oxygen.executable

import zio.*

trait ExecutableApp extends ZIOAppDefault {

  val executable: Executable

  override final def run: ZIO[ZIOAppArgs & Scope, Nothing, Unit] = ???
  // TODO (KR) :

}
