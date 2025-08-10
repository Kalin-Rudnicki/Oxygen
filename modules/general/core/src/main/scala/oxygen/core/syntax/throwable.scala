package oxygen.core.syntax

object throwable {

  extension (self: Throwable) {

    def safeGetMessage: String = Option(self.getMessage).getOrElse(self.toString)

  }

}
