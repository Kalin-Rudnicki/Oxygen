package oxygen.zio.system

import oxygen.predef.core.*
import oxygen.zio.error.FileSystemError
import oxygen.zio.syntax.error.*
import zio.*

trait FileSystem {
  def path(p: String): IO[FileSystemError, Path]
  def path(p: NonEmptyList[String]): IO[FileSystemError, Path]
}
object FileSystem extends FileSystemPlatformSpecific, FileSystemPlatformSpecificImpl {

  private[system] def attempt[A](path: => Path.PathName, whileAttemptingTo: => String)(thunk: => A)(using trace: Trace): IO[FileSystemError.GenericOperationError, A] =
    ZIO.attempting(FileSystemError.GenericOperationError(path, whileAttemptingTo, _)) { thunk }

  private[system] def unimplemented(path: String, operation: String)(using trace: Trace): IO[FileSystemError.Unimplemented, Nothing] =
    ZIO.fail(FileSystemError.Unimplemented(Path.PathName(path), operation))

  private[system] def unimplementedDie(path: String, operation: String)(using trace: Trace): IO[Nothing, Nothing] =
    ZIO.die(FileSystemError.Unimplemented(Path.PathName(path), operation))

  val current: FiberRef[FileSystem] = Unsafe.unsafely { FiberRef.unsafe.make { default } }

}
