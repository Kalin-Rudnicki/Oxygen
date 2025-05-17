package oxygen.core

import oxygen.core.collection.NonEmptyList
import oxygen.core.typeclass.SeqOps
import scala.collection.mutable

sealed trait IndentedString {

  //////////////////////////////////////////////////////////////////////////////////////////////////////
  //      Misc
  //////////////////////////////////////////////////////////////////////////////////////////////////////

  final def nonInlines: Seq[IndentedString.NonInline] = this match
    case self: IndentedString.NonInline  => Seq(self)
    case IndentedString.Inline(children) => children

  final def withoutInlineWrapper: IndentedString = this match
    case IndentedString.Inline(Seq(self)) => self
    case _                                => this

  final def prefix(prefix: String, joinUsing: String): IndentedString = this match
    case IndentedString.Str(str)                                 => IndentedString.Str(s"$prefix$joinUsing$str")
    case IndentedString.Inline(Seq(IndentedString.Str(str), t*)) => IndentedString.Inline(IndentedString.Str(s"$prefix$joinUsing$str") +: t)
    case _                                                       => IndentedString.inline(prefix, this)

  //////////////////////////////////////////////////////////////////////////////////////////////////////
  //      Show
  //////////////////////////////////////////////////////////////////////////////////////////////////////

  final def toString(idtStrs: NonEmptyList[String]): String = {
    val builder = mutable.StringBuilder()
    var first: Boolean = true

    inline def makeIndent(indent: String): Unit =
      if (first)
        first = false
      else {
        builder.append('\n')
        builder.append(indent)
      }

    def rec(indentedString: IndentedString, indent: String, nextIndents: NonEmptyList[String]): Unit =
      indentedString match {
        case IndentedString.Str(str) =>
          makeIndent(indent)
          builder.append(str)
        case IndentedString.Inline(children) =>
          children.foreach(rec(_, indent, nextIndents))
        case IndentedString.Indented(children) =>
          val newIndent = indent + nextIndents.head
          val newNext = NonEmptyList.fromList(nextIndents.tail).getOrElse(idtStrs)
          children.foreach(rec(_, newIndent, newNext))
        case IndentedString.Break =>
          makeIndent("")
      }

    rec(this, "", idtStrs)

    builder.toString
  }

  final def toString(idtStr0: String, idtStr1: String, idtStrN: String*): String =
    toString(NonEmptyList(idtStr0, idtStr1 :: idtStrN.toList))

  final def toString(idtStr: String): String = {
    val builder = mutable.StringBuilder()
    var first: Boolean = true

    inline def makeIndent(indent: Int): Unit =
      if (first)
        first = false
      else {
        builder.append('\n')
        var idt: Int = indent
        while (idt > 0) {
          builder.append(idtStr)
          idt -= 1
        }
      }

    def rec(indentedString: IndentedString, indent: Int): Unit = indentedString match
      case IndentedString.Str(str)           => makeIndent(indent); builder.append(str)
      case IndentedString.Indented(children) => val indent2 = indent + 1; children.foreach(rec(_, indent2))
      case IndentedString.Inline(children)   => children.foreach(rec(_, indent))
      case IndentedString.Break              => makeIndent(0)

    rec(this, 0)

    builder.toString
  }

  final def toStringColorized(idt: String, color0: Color, colorN: Color*): String =
    toString(NonEmptyList(color0, colorN.toList).map { c => idt.withFg(c).toString })

  final def toStringColorized(idt: String): String =
    toStringColorized(
      idt,
      Color.RGB.hex("#7D5BA6"),
      Color.RGB.hex("#E54724"),
      Color.RGB.hex("#55D6BE"),
      Color.RGB.hex("#3E8914"),
      Color.RGB.hex("#806FC3"),
    )

  override final def toString: String =
    toString("    ")

}
object IndentedString {

  //////////////////////////////////////////////////////////////////////////////////////////////////////
  //      ...
  //////////////////////////////////////////////////////////////////////////////////////////////////////

  sealed trait NonInline extends IndentedString

  case object Break extends IndentedString.NonInline

  final case class Str(str: String) extends IndentedString.NonInline

  final case class Inline(children: Seq[IndentedString.NonInline]) extends IndentedString

  final case class Indented(children: Seq[IndentedString.NonInline]) extends IndentedString.NonInline

  //////////////////////////////////////////////////////////////////////////////////////////////////////
  //      API
  //////////////////////////////////////////////////////////////////////////////////////////////////////

  def inline(children: IndentedString*): IndentedString =
    Inline(children.flatMap(_.nonInlines))

  def indented(children: IndentedString*): IndentedString =
    Indented(children.flatMap(_.nonInlines))

  def section(header: String)(body: IndentedString*): IndentedString =
    IndentedString.inline(header, IndentedString.indented(body*))

  def keyValue(key: String, value: String): IndentedString =
    if (value.contains('\n')) IndentedString.section(key)(value)
    else s"$key$value"

  def fromAny(any: Any)(show: PartialFunction[Matchable, IndentedString]): IndentedString = fromAny(any, show.lift)
  def fromAny(any: Any): IndentedString = fromAny(any, _ => None)

  //////////////////////////////////////////////////////////////////////////////////////////////////////
  //      Conversion
  //////////////////////////////////////////////////////////////////////////////////////////////////////

  trait ToIndentedString[-T] {
    def convert(t: T): IndentedString
  }
  object ToIndentedString {

    given id: ToIndentedString[IndentedString] =
      identity(_)

    given string: ToIndentedString[String] =
      str =>
        if (str.contains('\n')) Inline(str.split('\n').map(Str(_)).toSeq)
        else Str(str)

    given option: [A] => (toIdtStr: ToIndentedString[A]) => ToIndentedString[Option[A]] = {
      case Some(value) => toIdtStr.convert(value)
      case None        => IndentedString.Inline(Nil)
    }

    given seq: [S[_], A] => (seqOps: SeqOps[S], a: ToIndentedString[A]) => ToIndentedString[S[A]] =
      as => Inline(seqOps.newIterator(as).map(a.convert).toSeq.flatMap(_.nonInlines)).withoutInlineWrapper

  }

  given convertIdtStr: [A] => (toIdtStr: ToIndentedString[A]) => Conversion[A, IndentedString] =
    toIdtStr.convert(_)

  //////////////////////////////////////////////////////////////////////////////////////////////////////
  //      Helpers
  //////////////////////////////////////////////////////////////////////////////////////////////////////

  private def fromAnyProduct(product: Product, show: Matchable => Option[IndentedString]): IndentedString =
    IndentedString.inline(
      s"[${product.productPrefix}]:",
      IndentedString.indented(
        product.productElementNames
          .zip(product.productIterator)
          .toSeq
          .map { case (key, value) => fromAny(value, show).prefix(s"$key:", " ") }*,
      ),
    )
  private def fromAny(any: Any, show: Matchable => Option[IndentedString]): IndentedString = {
    val matchable: Matchable = any.asInstanceOf[Matchable]
    show(matchable) match {
      case Some(is) => is
      case None =>
        matchable match {
          case option: Option[?] => fromAnyProduct(option, show)
          case seq: List[?]      => IndentedString.section("[List[_]]:")(seq.map(fromAny(_, show))*)
          case seq: Vector[?]    => IndentedString.section("[Vector[_]]:")(seq.map(fromAny(_, show))*)
          case seq: Seq[?]       => IndentedString.section("[Seq[_]]:")(seq.map(fromAny(_, show))*)
          case seq: Array[?]     => IndentedString.section("[Array[_]]:")(seq.map(fromAny(_, show))*)
          case product: Product  => fromAnyProduct(product, show)
          case _                 => IndentedString.Str(any.toString)
        }
    }
  }

}
