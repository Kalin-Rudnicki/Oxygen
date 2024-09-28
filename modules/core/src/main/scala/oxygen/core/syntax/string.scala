package oxygen.core.syntax

import oxygen.core.syntax.option.*
import scala.util.matching.Regex

object string {

  private val camelRegex: Regex = "\\d+|[A-Z](?=[a-z])|[A-Z]+(?:(?=$)|(?![^A-Z]))".r
  private val snakeRegex: Regex = "_[a-zA-Z0-9]".r

  private val splitStartTag = "\u1111\u2222\u3333"
  private val splitEndTag = "\u3333\u2222\u1111"

  enum Alignment { case Left, Center, Right }

  extension (self: String) {

    // =====| Misc |=====

    def toNES: Option[String] =
      Option.when(self.nonEmpty)(self)

    def pluralize(amount: Long, pluralSuffix: String = "s", singularSuffix: String = ""): String =
      s"$self${if (amount.abs == 1) singularSuffix else pluralSuffix}"

    def decapitalize: String =
      if (self == null || self.length == 0 || !self.charAt(0).isUpper) self
      else self.updated(0, self.charAt(0).toLower)

    def detailedSplit(regex: Regex): (Boolean, Seq[String], Boolean) = {
      val raw = regex.split(s"$splitStartTag$self$splitEndTag").toSeq
      if (raw.length < 2) (false, Seq(self), false)
      else {
        val a = raw.head
        val b = raw.last
        val a2 = a.stripPrefix(splitStartTag)
        val b2 = b.stripSuffix(splitEndTag)

        // TODO (KR) : this part could probably be made a bit more efficient
        val updatedRaw = raw.updated(0, a2).updated(raw.length - 1, b2)
        (a2.length, b2.length) match {
          case (0, 0) => (true, updatedRaw.slice(1, raw.length - 1), true)
          case (0, _) => (true, updatedRaw.tail, false)
          case (_, 0) => (false, updatedRaw.init, true)
          case _      => (false, updatedRaw, false)
        }
      }
    }
    def detailedSplit(regex: Regex, includeEmptyLeading: Boolean, includeEmptyTrailing: Boolean): Seq[String] = {
      val (hasEmptyLeading, strs, hasEmptyTrailing) = detailedSplit(regex)
      val tmp = if (hasEmptyLeading && includeEmptyLeading) "" +: strs else strs
      if (hasEmptyTrailing && includeEmptyTrailing) tmp :+ "" else tmp
    }

    inline def unesc: String = self.unesc("\"")
    inline def unesc(leftAndRight: String): String = self.unesc(leftAndRight, leftAndRight)
    def unesc(left: String, right: String): String = s"$left${self.toList.map(_.unesc("")).mkString}$right"

    // =====| Alignment |=====

    private def alignFunction(length: Int, char: Char)(padF: Int => (Option[Int], Option[Int])): String = {
      val toAdd = length - self.length
      val charStr = char.toString
      if (toAdd > 0) {
        val (left, right) = padF(toAdd)
        List(left.map(charStr * _), self.some, right.map(charStr * _)).flatten.mkString
      } else self
    }

    def alignLeft(length: Int, char: Char): String =
      alignFunction(length, char)(toAdd => (None, toAdd.some))
    def alignLeft(length: Int): String =
      alignLeft(length, ' ')

    def alignRight(length: Int, char: Char): String =
      alignFunction(length, char)(toAdd => (toAdd.some, None))
    def alignRight(length: Int): String =
      alignRight(length, ' ')

    def alignCenter(length: Int, char: Char): String =
      alignFunction(length, char) { toAdd =>
        val left = toAdd / 2
        val right = toAdd - left
        (left.some, right.some)
      }
    def alignCenter(length: Int): String =
      alignCenter(length, ' ')

    def align(length: Int, alignment: Alignment, char: Char): String =
      alignment match {
        case Alignment.Left   => self.alignLeft(length, char)
        case Alignment.Center => self.alignCenter(length, char)
        case Alignment.Right  => self.alignRight(length, char)
      }
    def align(length: Int, alignment: Alignment): String =
      align(length, alignment, ' ')

    // =====| Color |=====

    def stripColor: String =
      self.replaceAll("\u001b\\[\\d+(;\\d+)*m", "")

    // =====| Casing |=====

    // --- Camel -> ___ ---

    def camelToSnake: String =
      camelRegex.replaceAllIn(self, m => if (m.start == 0) m.matched else s"_${m.matched}").toLowerCase

    def camelToDash: String =
      camelToSnake.snakeToDash

    // --- Snake -> ___ ---

    def snakeToDash: String =
      self.replace('_', '-')

    def snakeToLowerCamel: String =
      snakeRegex.replaceAllIn(self, _.matched.substring(1).capitalize)

    def snakeToUpperCamel: String =
      self.snakeToLowerCamel.capitalize

    // --- Dash -> ___ ---

    def dashToSnake: String =
      self.replace('-', '_')

    def dashToLowerCamel: String =
      dashToSnake.snakeToLowerCamel

    def dashToUpperCamel: String =
      dashToSnake.snakeToUpperCamel

  }

  extension (self: Char) {

    inline def unesc: String = self.unesc("'")

    inline def unesc(leftAndRight: String): String = self.unesc(leftAndRight, leftAndRight)

    def unesc(left: String, right: String): String = {
      val charText =
        self match {
          case '\n' => "\\n"
          case '\\' => "\\\\"
          case '\t' => "\\t"
          case '"'  => "\\\""
          case c    => c.toString
        }
      s"$left$charText$right"
    }

  }

}
