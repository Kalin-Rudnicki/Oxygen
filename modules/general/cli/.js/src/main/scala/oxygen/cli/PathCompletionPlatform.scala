package oxygen.cli

import zio.*

private[cli] object PathCompletionPlatform {

  def completeUnderBase(@scala.annotation.unused basePath: String, @scala.annotation.unused in: String): Task[Seq[String]] =
    ZIO.succeed(Nil)

}
