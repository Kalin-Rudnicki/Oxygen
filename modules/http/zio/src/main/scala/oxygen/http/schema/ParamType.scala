package oxygen.http.schema

object ParamType {

  enum Path {
    case Single extends Path
    case NonEmptyRest extends Path
    case Rest extends Path
  }

  enum Param(final val allowsEmpty: Boolean, final val allowsMany: Boolean) {
    case Required extends Param(false, false)
    case Optional extends Param(true, false)
    case ManyRequired extends Param(false, true)
    case ManyOptional extends Param(true, true)
  }

}
