package oxygen.ui.web.internal

import org.scalajs.dom.{document, CanvasRenderingContext2D, Element, HTMLCanvasElement, HTMLElement}
import oxygen.predef.core.*
import scalajs.js.Dynamic
import zio.*

object Renderer {

  //////////////////////////////////////////////////////////////////////////////////////////////////////
  //      API
  //////////////////////////////////////////////////////////////////////////////////////////////////////

  def renderBody(elements: => Growable[DOMElement]): URIO[Scope, Unit] =
    currentBody.update {
      case Some(currentBody) =>
        diff(currentBody, makeBody(elements)).asInstanceOf[RenderedEntity.Node].some
      case None =>
        val rendered: RenderedEntity.Node = render(makeBody(elements)).asInstanceOf[RenderedEntity.Node]
        document.body = rendered.rendered.asInstanceOf[HTMLElement]
        rendered.some
    }

  //////////////////////////////////////////////////////////////////////////////////////////////////////
  //      Internal
  //////////////////////////////////////////////////////////////////////////////////////////////////////

  private given Unsafe = Unsafe.unsafe(identity)

  private val currentBody: Ref[Option[RenderedEntity.WithTagAndChildren]] = Ref.Synchronized.unsafe.make(None)

  private def makeBody(elements: => Growable[DOMElement]): DOMElement.Node =
    elements.toArraySeq match
      case ArraySeq(node: DOMElement.Node) if node.tag == "body" => node
      case other                                                 => DOMElement.Node("body", other)

  private def diff(currentEntity: RenderedEntity, newEntity: DOMElement.Entity): RenderedEntity =
    (currentEntity, newEntity) match {
      case (currentEntity: RenderedEntity.Node, newEntity: DOMElement.Node) if currentEntity.preRendered.tag == newEntity.tag =>
        val newChildren = patch(currentEntity, newEntity)
        RenderedEntity.Node(newEntity, currentEntity.rendered, newChildren, newEntity.cssStr, newEntity.htmlAttrMap, newEntity.objectAttrMap)
      case (currentEntity: RenderedEntity.Text, newEntity: DOMElement.Text) if currentEntity.preRendered.text == newEntity.text =>
        currentEntity
      case (currentEntity: RenderedEntity.Text, newEntity: DOMElement.Text) =>
        currentEntity.rendered.textContent = newEntity.text
        RenderedEntity.Text(currentEntity.rendered, newEntity)
      case (currentEntity: RenderedEntity.Canvas, newEntity: DOMElement.Canvas) =>
        val newChildren = patch(currentEntity, newEntity)
        newEntity.draw(currentEntity.ctx)
        RenderedEntity.Canvas(newEntity, currentEntity.rendered, currentEntity.ctx, newChildren, newEntity.cssStr, newEntity.htmlAttrMap, newEntity.objectAttrMap)
      case _ =>
        val newRendered: RenderedEntity = render(newEntity)
        currentEntity.rendered.parentNode.replaceChild(newRendered.rendered, currentEntity.rendered)
        newRendered
    }

  private def patch(currentEntity: RenderedEntity.WithTagAndChildren, newEntity: DOMElement.WithTagAndChildren): ArraySeq[RenderedEntity] = {
    Ior.zippedMapIterator(currentEntity.htmlAttrMap, newEntity.htmlAttrMap).foreach {
      case (k, Ior.Both(before, after)) =>
        if (before != after)
          currentEntity.rendered.setAttribute(k, after)
      case (k, Ior.Right(after)) =>
        currentEntity.rendered.setAttribute(k, after)
      case (k, Ior.Left(_)) =>
        currentEntity.rendered.removeAttribute(k)
    }
    Ior.zippedMapIterator(currentEntity.objectAttrMap, newEntity.objectAttrMap).foreach {
      case (k, Ior.Both(before, after)) =>
        if (before != after)
          currentEntity.renderedDynamic.updateDynamic(k)(after)
      case (k, Ior.Right(after)) =>
        currentEntity.renderedDynamic.updateDynamic(k)(after)
      case (k, Ior.Left(_)) =>
        currentEntity.renderedDynamic.updateDynamic(k)(null)
    }
    (currentEntity.cssStr, newEntity.cssStr) match {
      case (Some(before), Some(after)) =>
        if (before != after)
          currentEntity.rendered.setAttribute("style", after)
      case (None, None) =>
        ()
      case (None, Some(after)) =>
        currentEntity.rendered.setAttribute("style", after)
      case (Some(_), None) =>
        currentEntity.rendered.removeAttribute("style")
    }

    val minLen = currentEntity.children.length min newEntity.entityChildren.length
    val array: Array[RenderedEntity] = new Array(newEntity.entityChildren.length)

    var idx: Int = 0

    while (idx < minLen) {
      val before = currentEntity.children(idx)
      val after = newEntity.entityChildren(idx)

      array(idx) = diff(before, after)

      idx = idx + 1
    }
    while (idx < newEntity.entityChildren.length) {
      val after = render(newEntity.entityChildren(idx))
      currentEntity.rendered.appendChild(after.rendered)
      array(idx) = after
      idx = idx + 1
    }
    while (idx < currentEntity.children.length) {
      currentEntity.rendered.removeChild(currentEntity.children(idx).rendered)
      idx = idx + 1
    }

    ArraySeq.unsafeWrapArray(array)
  }

  private def render(entity: DOMElement.Entity): RenderedEntity = entity match
    case text: DOMElement.Text              => RenderedEntity.Text(document.createTextNode(text.text), text)
    case tac: DOMElement.WithTagAndChildren => render(tac)

  private def render(tac: DOMElement.WithTagAndChildren): RenderedEntity.WithTagAndChildren = {
    val node: Element = document.createElement(tac.tag)
    val nodeDynamic: Dynamic = node.asInstanceOf[Dynamic]

    tac.htmlAttrMap.foreach { case (k, v) => node.setAttribute(k, v) }
    tac.objectAttrMap.foreach { case (k, v) => nodeDynamic.updateDynamic(k)(v) }
    tac.cssStr.foreach { css => node.setAttribute("style", css) }

    val builtChildren: ArraySeq[RenderedEntity] = tac.entityChildren.map(render)

    builtChildren.foreach { e => node.appendChild(e.rendered) }

    tac match {
      case tac: DOMElement.Node =>
        RenderedEntity.Node(
          tac,
          node,
          builtChildren,
          tac.cssStr,
          tac.htmlAttrMap,
          tac.objectAttrMap,
        )
      case tac: DOMElement.Canvas =>
        val typedNode: HTMLCanvasElement = node.asInstanceOf[HTMLCanvasElement]
        val ctx: CanvasRenderingContext2D = typedNode.getContext("2d").asInstanceOf[CanvasRenderingContext2D]

        tac.draw(ctx)

        RenderedEntity.Canvas(
          tac,
          typedNode,
          ctx,
          builtChildren,
          tac.cssStr,
          tac.htmlAttrMap,
          tac.objectAttrMap,
        )
    }
  }

}
