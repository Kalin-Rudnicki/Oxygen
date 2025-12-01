package oxygen.ui.web.create

import oxygen.meta.*
import oxygen.predef.core.*
import oxygen.quoted.*
import oxygen.ui.web.internal.StyleSheetElement
import scala.annotation.tailrec
import scala.language.dynamics
import scala.quoted.*

final case class StyleSheet(header: String, body: () => String) {

  val styleSheetId: String = s"oxygen-css-style : $header"

  def innerHTML: String = s"/* =====| $header |===== */\n\n${body()}"

  override def toString: String = innerHTML

}
object StyleSheet {

  def makeLazy(header: String)(body: => String): StyleSheet =
    StyleSheet(header, () => body)

  def makeConst(header: String)(body: String): StyleSheet =
    StyleSheet(header, () => body)

  def compile(header: String, elems: => Seq[Growable[StyleSheetElement.AppliedStyleSheet]]): StyleSheet = {
    @tailrec
    def loop(
        current: Option[(String, Growable[(String, String)])],
        queue: List[StyleSheetElement.Leaf],
        acc: Growable[(String, Growable[(String, String)])],
    ): Growable[(String, Growable[(String, String)])] =
      queue match {
        case head :: tail =>
          current match {
            case Some((currentKey, currentElems)) if head.selectorString == currentKey =>
              loop((currentKey, currentElems :+ (head.key, head.value)).some, tail, acc)
            case Some(current) =>
              loop((head.selectorString, Growable.single((head.key, head.value))).some, tail, acc :+ current)
            case None =>
              loop((head.selectorString, Growable.single((head.key, head.value))).some, tail, acc)
          }
        case Nil =>
          acc ++ Growable.option(current)
      }

    StyleSheet.makeLazy(header)(
      loop(
        None,
        Growable.many(elems).flatten.flatMap(_.leafs).toArraySeq.sortBy(_.loc.line).toList,
        Growable.empty,
      ).map { case (selector, pairs) =>
        s"$selector {${pairs.map { case (k, v) => s"\n  $k: $v;" }.to[Seq].mkString}\n}"
      }.to[Seq]
        .mkString("\n\n"),
    )
  }

  def variables(header: String, scope: String = ":root")(vars: (CSSVar, String)*): StyleSheet =
    StyleSheet.makeLazy(s"variables : $header")(
      s"$scope {${vars.map { case (k, v) => s"\n  ${k.name}: $v;" }.mkString}\n}",
    )

  private def fetchAll(cdef: ClassDef, termAcc: Term)(using Quotes): Growable[Expr[Growable[StyleSheetElement.AppliedStyleSheet]]] = {
    val statements: ArraySeq[Statement] = cdef.body.toArraySeq

    val valDefs: ArraySeq[ValDef] = statements.collect { case s: ValDef => s }
    val classDefs: ArraySeq[ClassDef] = statements.collect { case s: ClassDef => s }

    val valDefMap: Map[String, ValDef] = valDefs.map { v => (v.name, v) }.toMap

    val pairs: ArraySeq[(ClassDef, ValDef)] =
      classDefs.collect { case c if c.name.endsWith("$") => valDefMap.get(c.name.stripSuffix("$")).map((c, _)) }.flatten

    val self: Growable[Expr[Growable[StyleSheetElement.AppliedStyleSheet]]] =
      if termAcc.tpe <:< TypeRepr.of[StyleSheetBuilder.HasBuild] then Growable.single('{ ${ termAcc.asExprOf[StyleSheetBuilder.HasBuild] }.built })
      else Growable.empty
    val children: Growable[Expr[Growable[StyleSheetElement.AppliedStyleSheet]]] =
      Growable.many(pairs).flatMap { case (cdef, vdef) => fetchAll(cdef, termAcc.select(vdef.symbol)) }

    self ++ children
  }

  private def derivedImpl[A <: StyleSheetBuilder: Type](using Quotes): Expr[StyleSheet] = {
    val aTypeRepr = TypeRepr.of[A]

    if !aTypeRepr.isSingleton then report.errorAndAbort("not an object")

    val termTree = aTypeRepr.termSymbol.tree.narrow[ValDef]

    val all: Growable[Expr[Growable[StyleSheetElement.AppliedStyleSheet]]] =
      fetchAll(aTypeRepr.typeSymbol.tree.narrow[ClassDef], termTree.valRef)

    val res =
      '{ StyleSheet.compile(${ Expr(s"From StyleSheet : ${termTree.name}") }, ${ all.seqToExprOf[Seq] }) }

    res
  }

  inline def derived[A <: StyleSheetBuilder]: StyleSheet = ${ derivedImpl[A] }

}
