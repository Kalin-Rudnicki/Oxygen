package oxygen.core.syntax

import oxygen.core.typeclass.Show
import scala.collection.mutable

object show {

  extension [A](self: A)
    def show(using s: Show[A]): String = s.show(self)
    def toShown(using s: Show[A]): Show.Shown = Show.Shown.show(self)
    def toText: oxygen.core.Text = oxygen.core.Text.fromAny(self)

  extension (self: String)
    def wrapShown: Show.Shown =
      Show.Shown(self)

  extension (sc: StringContext) {

    def sh(args: (String | Show.Shown)*): String = showSimple(args*)
    def shs(args: Show.Shown*): String = showStrict(args*)

    /**
      * String: interpolated as raw strings (will not have quotes)
      * everything else: implicitly converted to Shown via a Show instance
      *
      * a String can have quotes here using [[toShown]]
      */
    def showSimple(args: (String | Show.Shown)*): String = {
      val strings = sc.parts.iterator
      val expressions = args.iterator
      val b = new mutable.StringBuilder(strings.next())
      while strings.hasNext do {
        b.append(
          expressions.next() match
            case str: String       => str
            case Show.Shown(value) => value,
        )
        b.append(strings.next())
      }
      b.toString()
    }

    /**
      * String: implicitly converted to Shown via a Show instance (will have quotes)
      * everything else: implicitly converted to Shown via a Show instance
      *
      * a string can not have quotes here using [[wrapShown]]
      */
    def showStrict(args: Show.Shown*): String = {
      val strings = sc.parts.iterator
      val expressions = args.iterator
      val b = new mutable.StringBuilder(strings.next())
      while strings.hasNext do {
        b.append(expressions.next().value)
        b.append(strings.next())
      }
      b.toString()
    }

  }

}
