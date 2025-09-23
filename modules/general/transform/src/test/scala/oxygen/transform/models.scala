package oxygen.transform

object domain {

  final case class Email(email: String)

  final case class Password(password: String)

  final case class Person(
      first: String,
      last: String,
      email: Email,
      password: Password,
  )

  enum SumExample {
    case A(s: String, b: Boolean, i: Int)
    case B(value: Int)
  }

}

object api {

  final case class Person(
      first: String,
      last: String,
      email: String,
      password: String,
  )

  enum SumExample {
    case A(s: String, b: Option[Boolean])
    case B
    case C
  }
}
