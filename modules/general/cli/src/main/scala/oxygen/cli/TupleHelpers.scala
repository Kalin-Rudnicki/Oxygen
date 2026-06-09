package oxygen.cli

private[cli] object TupleHelpers {

  def fromNested3[A, B, C](nested: Any): (A, B, C) =
    val ((a, b), c) = nested.asInstanceOf[((A, B), C)]
    (a, b, c)

  def fromNested4[A, B, C, D](nested: Any): (A, B, C, D) =
    val (((a, b), c), d) = nested.asInstanceOf[(((A, B), C), D)]
    (a, b, c, d)

}
