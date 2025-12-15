package oxygen.ui.web

import zio.*

final case class PageMessages(
    pageMessages: Chunk[PageMessage],
) {

  def add(message: PageMessage): PageMessages = PageMessages(pageMessages :+ message)
  def addAll(messages: IterableOnce[PageMessage]): PageMessages = PageMessages(pageMessages :++ messages)

  def remove(message: PageMessage): PageMessages = PageMessages(pageMessages.filterNot(_.id == message.id))
  def removeAll(messages: IterableOnce[PageMessage]): PageMessages = {
    val toRemove = messages.iterator.map(_.id).toSet
    PageMessages(pageMessages.filterNot(m => toRemove.contains(m.id)))
  }

  def :+(message: PageMessage): PageMessages = add(message)
  def :++(messages: IterableOnce[PageMessage]): PageMessages = addAll(messages)

  def -(message: PageMessage): PageMessages = remove(message)
  def --(messages: IterableOnce[PageMessage]): PageMessages = removeAll(messages)

  def clear: PageMessages = PageMessages(Chunk.empty)

}
object PageMessages {

  // TODO (KR) : helper functions for displaying messages

  object PageLocal extends PageLocalState[PageMessages]("PageMessages")(PageMessages(Chunk.empty))

  def schedule(message: PageMessage, duration: Duration): UIO[Unit] =
    ZIO
      .uninterruptibleMask { restore =>
        restore(PageLocal.update(_ :+ message)) *>
          Clock.sleep(duration) *>
          PageLocal.update(_ - message)
      }
      .forkDaemon
      .unit

  def add(message: PageMessage): UIO[Unit] = PageLocal.update(_ :+ message)
  def addAll(messages: IterableOnce[PageMessage]): UIO[Unit] = PageLocal.update(_ :++ messages)

  def remove(message: PageMessage): UIO[Unit] = PageLocal.update(_ - message)
  def removeAll(messages: IterableOnce[PageMessage]): UIO[Unit] = PageLocal.update(_ -- messages)

  def :+(message: PageMessage): UIO[Unit] = PageLocal.update(_ :+ message)
  def :++(messages: IterableOnce[PageMessage]): UIO[Unit] = PageLocal.update(_ :++ messages)

  def -(message: PageMessage): UIO[Unit] = PageLocal.update(_ - message)
  def --(messages: IterableOnce[PageMessage]): UIO[Unit] = PageLocal.update(_ -- messages)

}
