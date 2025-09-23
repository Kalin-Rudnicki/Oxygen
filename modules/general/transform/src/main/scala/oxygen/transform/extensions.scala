package oxygen.transform

extension [From](from: From)
  def transformInto[To](using transform: Transform[From, To]): To =
    transform.transform(from)
