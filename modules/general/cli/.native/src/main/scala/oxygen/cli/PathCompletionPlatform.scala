package oxygen.cli

import java.nio.file.{Files, Paths}
import scala.jdk.CollectionConverters.*
import zio.*

private[cli] object PathCompletionPlatform {

  def completeUnderBase(basePath: String, in: String): Task[Seq[String]] =
    ZIO.attempt {
      val base = Paths.get(basePath).toAbsolutePath.normalize()

      def listAt(relativeIn: String): Seq[String] =
        val (parent, prefix) = PathCompletion.parentAndPrefix(relativeIn)
        val dir = base.resolve(parent.stripPrefix("/")).normalize()
        if !dir.startsWith(base) || !Files.isDirectory(dir) then Nil
        else
          val entries =
            Files.list(dir).iterator().asScala.toList
              .map(_.getFileName.toString)
              .sorted
          val isDirectory: Map[String, Boolean] =
            entries.map { name =>
              name -> Files.isDirectory(dir.resolve(name))
            }.toMap
          PathCompletion.buildCompletions(parent, prefix, entries, name => isDirectory.getOrElse(name, false))

      PathCompletion.expandSingleMatch(listAt(in), listAt)
    }.catchAll(_ => ZIO.succeed(Nil))

}
