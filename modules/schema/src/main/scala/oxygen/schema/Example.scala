package oxygen.schema

final case class Example[A, Encoded](
    value: A,
    encoded: Encoded,
)
