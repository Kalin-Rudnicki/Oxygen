package oxygen.executable
sealed trait AutoCompleteError extends Throwable
object AutoCompleteError {
  final case class Help(message: String) extends AutoCompleteError { override def getMessage = message }
  final case class ProgramError(cause: Throwable) extends AutoCompleteError {
    override def getMessage = cause.getMessage
    override def getCause = cause
  }
}
