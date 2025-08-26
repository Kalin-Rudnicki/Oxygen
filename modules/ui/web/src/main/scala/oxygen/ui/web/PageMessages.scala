package oxygen.ui.web

import zio.Chunk

final case class PageMessages(
    pageMessages: Chunk[PageMessage],
) {

  def -(message: PageMessage): PageMessages = PageMessages(pageMessages.filterNot(_.id == message.id))
  def --(messages: IterableOnce[PageMessage]): PageMessages = {
    val toRemove = messages.iterator.map(_.id).toSet
    PageMessages(pageMessages.filterNot(m => toRemove.contains(m.id)))
  }

  def :+(message: PageMessage): PageMessages = PageMessages(pageMessages :+ message)
  def :++(messages: IterableOnce[PageMessage]): PageMessages = PageMessages(pageMessages :++ messages)

}
object PageMessages {

  // TODO (KR) : helper functions for displaying messages

  object PageLocal extends PageLocalState[PageMessages]("PageMessages")(PageMessages(Chunk.empty))

}
