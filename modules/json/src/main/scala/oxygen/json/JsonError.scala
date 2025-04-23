package oxygen.json

final case class JsonError(rPath: List[JsonError.Path], cause: JsonError.Cause) extends Throwable {

  def inField(name: String): JsonError = JsonError(JsonError.Path.Field(name) :: rPath, cause)
  def atIndex(index: Int): JsonError = JsonError(JsonError.Path.Index(index) :: rPath, cause)

  override def getMessage: String =
    rPath.reverse match
      case head :: tail => s"${head.showFirst}${tail.map(_.showNonFirst).mkString} : ${cause.show}"
      case Nil          => cause.show

  override def toString: String = getMessage

}
object JsonError {

  enum Path {

    case Field(name: String)
    case Index(index: Int)

    final def showFirst: String = this match
      case Path.Field(name)  => name
      case Path.Index(index) => s"_root_[$index]"

    final def showNonFirst: String = this match
      case Path.Field(name)  => "." + name
      case Path.Index(index) => s"[$index]"

  }

  enum Cause {

    case MissingRequired
    case InvalidKey(message: String)
    case DecodingFailed(message: String)
    case InvalidType(expected: Json.Type, actual: Json.Type)
    case InvalidJson(idx: Int)

    final def show: String = this match
      case Cause.DecodingFailed(message)       => message
      case Cause.InvalidKey(message)           => s"Invalid key: $message"
      case Cause.MissingRequired               => "Missing required value"
      case Cause.InvalidType(expected, actual) => s"Invalid type, expected `$expected`, but got `$actual`"
      case Cause.InvalidJson(idx)              => s"Invalid json at idx $idx"

  }

}
