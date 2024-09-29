package oxygen.zio.logger

trait WithLogLevelAbstract[A] {

  protected val make: LogLevel => A

  final def apply(level: LogLevel): A = make(level)

  final def never: A = make(LogLevel.Never)
  final def trace: A = make(LogLevel.Trace)
  final def debug: A = make(LogLevel.Debug)
  final def detailed: A = make(LogLevel.Detailed)
  final def info: A = make(LogLevel.Info)
  final def important: A = make(LogLevel.Important)
  final def warning: A = make(LogLevel.Warning)
  final def error: A = make(LogLevel.Error)
  final def fatal: A = make(LogLevel.Fatal)
  final def always: A = make(LogLevel.Always)

}

trait WithLogLevel[A](override protected val make: LogLevel => A) extends WithLogLevelAbstract[A]
object WithLogLevel {
  def make[A](f: LogLevel => A): WithLogLevel[A] = new WithLogLevel[A](f) {}
}
