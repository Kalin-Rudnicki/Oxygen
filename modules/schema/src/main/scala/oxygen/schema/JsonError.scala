package oxygen.schema

final case class JsonError(rPath: List[JsonError.Path], cause: JsonError.Cause) extends Throwable {

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
    case DecodingFailed(message: String)

    final def show: String = this match
      case Cause.MissingRequired         => "Missing required value"
      case Cause.DecodingFailed(message) => message

  }

}
