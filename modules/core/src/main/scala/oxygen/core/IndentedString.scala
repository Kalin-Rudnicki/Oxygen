package oxygen.core

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

  def fromAny(any: Any)(show: PartialFunction[Matchable, IndentedString]): IndentedString = fromAny(any, show.lift)
  def fromAny(any: Any): IndentedString = fromAny(any, _ => None)

  //////////////////////////////////////////////////////////////////////////////////////////////////////
  //      Conversion
  //////////////////////////////////////////////////////////////////////////////////////////////////////

  trait ToIndentedString[-T] {
    def convert(t: T): IndentedString
  }

  implicit def convert[T: ToIndentedString](t: T): IndentedString =
    implicitly[ToIndentedString[T]].convert(t)

  implicit val indentedStringToIndentedString: ToIndentedString[IndentedString] = identity(_)

  implicit val stringToIndentedString: ToIndentedString[String] = Str(_)

  implicit def optionToIndentedString[T: ToIndentedString]: ToIndentedString[Option[? <: T]] = { opt =>
    val toIdtStr = implicitly[ToIndentedString[T]]
    Inline(opt.map(toIdtStr.convert).toList.flatMap(_.nonInlines)).withoutInlineWrapper
  }

  implicit def listToIndentedString[T: ToIndentedString]: ToIndentedString[List[? <: T]] = { list =>
    val toIdtStr = implicitly[ToIndentedString[T]]
    Inline(list.map(toIdtStr.convert).flatMap(_.nonInlines)).withoutInlineWrapper
  }

  implicit def seqToIndentedString[T: ToIndentedString]: ToIndentedString[Seq[? <: T]] = { seq =>
    val toIdtStr = implicitly[ToIndentedString[T]]
    Inline(seq.map(toIdtStr.convert).flatMap(_.nonInlines)).withoutInlineWrapper
  }

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
