package oxygen.core.syntax

object option {

  extension [A](inline self: A) {

    inline def some: Option[A] = Some(self)

  }

  extension [A](self: A) {

    inline def someWhen(f: A => Boolean): Option[A] = Option.when(f(self))(self)

  }

}
