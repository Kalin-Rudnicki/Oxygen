package oxygen.zio.system

import oxygen.zio.error.CommandError
import zio.*

object JavaCommandService extends CommandService {

  override def executeToOutputs(command: Command2): IO[CommandError, (stdOut: String, stdErr: String, exitCode: RuntimeFlags)] =
    ??? // FIX-PRE-MERGE (KR) :

}
