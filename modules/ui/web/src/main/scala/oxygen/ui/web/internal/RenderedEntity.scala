package oxygen.ui.web.internal

import org.scalajs.dom.{CanvasRenderingContext2D, Element, HTMLCanvasElement, Node, Text as TextNode}
import oxygen.predef.core.*
import scala.scalajs.js

sealed trait RenderedEntity {
  val preRendered: DOMElement.Entity
  val rendered: Node
}
object RenderedEntity {

  sealed trait WithTagAndChildren extends RenderedEntity {

    override val preRendered: DOMElement.WithTagAndChildren
    override val rendered: Element

    final lazy val renderedDynamic: js.Dynamic = rendered.asInstanceOf[js.Dynamic]

    val children: ArraySeq[RenderedEntity]
    val cssStr: Option[String]
    val htmlAttrMap: Map[String, String]
    val objectAttrMap: Map[String, js.Any]
    val classes: Set[String]

  }

  final case class Text(
      rendered: TextNode,
      preRendered: DOMElement.Text,
  ) extends RenderedEntity

  final case class Node(
      preRendered: DOMElement.Node,
      rendered: Element,
      children: ArraySeq[RenderedEntity],
      cssStr: Option[String],
      htmlAttrMap: Map[String, String],
      objectAttrMap: Map[String, js.Any],
      classes: Set[String],
  ) extends RenderedEntity.WithTagAndChildren

  final case class Canvas(
      preRendered: DOMElement.Canvas,
      rendered: HTMLCanvasElement,
      ctx: CanvasRenderingContext2D,
      children: ArraySeq[RenderedEntity],
      cssStr: Option[String],
      htmlAttrMap: Map[String, String],
      objectAttrMap: Map[String, js.Any],
      classes: Set[String],
  ) extends RenderedEntity.WithTagAndChildren

}
