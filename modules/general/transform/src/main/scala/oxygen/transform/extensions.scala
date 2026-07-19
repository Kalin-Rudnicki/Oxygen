package oxygen.transform

extension [From](from: From)
  def transformInto[To](using transform: Transform[From, To]): To =
    transform.transform(from)
  def transformIntoOrFail[To](using transform: TransformOrFail[From, To]): Either[TransformError, To] =
    transform.transformOrFail(from)
