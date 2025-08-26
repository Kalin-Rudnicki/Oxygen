package oxygen.ui.web.internal

import org.scalajs.dom.CanvasRenderingContext2D
import oxygen.predef.core.*
import scala.scalajs.js

sealed trait DOMElement
object DOMElement {

  sealed trait Entity extends DOMElement

  sealed trait WithTagAndChildren extends Entity {
    val tag: String
    val children: ArraySeq[DOMElement]

    final lazy val entityChildren: ArraySeq[DOMElement.Entity] = children.collect { case e: DOMElement.Entity => e }
    private final lazy val attrChildren: ArraySeq[DOMElement.Attr] = children.collect { case e: DOMElement.Attr => e }

    private final lazy val cssAttrMap: Map[String, String] = attrChildren.iterator.collect { case DOMElement.CSSAttr(k, v) => (k, v) }.toMap

    final lazy val cssStr: Option[String] = Option.when(cssAttrMap.nonEmpty)(cssAttrMap.iterator.map { case (k, v) => s"$k: $v" }.mkString("; "))
    final lazy val htmlAttrMap: Map[String, String] = attrChildren.iterator.collect { case DOMElement.HtmlAttr(k, v) => (k, v) }.toMap
    final lazy val objectAttrMap: Map[String, js.Any] = attrChildren.iterator.collect { case DOMElement.ObjectAttr(k, v) => (k, v) }.toMap

  }

  final case class Text(text: String) extends DOMElement.Entity

  final case class Node(
      tag: String,
      children: ArraySeq[DOMElement],
  ) extends DOMElement.WithTagAndChildren

  // TODO (KR) : needs sizing?
  final case class Canvas(draw: CanvasRenderingContext2D => Unit, children: ArraySeq[DOMElement]) extends DOMElement.WithTagAndChildren {
    override val tag: String = "canvas"
  }

  sealed trait Attr extends DOMElement
  final case class CSSAttr(key: String, value: String) extends DOMElement.Attr
  final case class HtmlAttr(key: String, value: String) extends DOMElement.Attr
  final case class ObjectAttr(key: String, value: js.Any) extends DOMElement.Attr

}
