package oxygen.ui.web.internal

import java.util.concurrent.atomic.AtomicReference
import oxygen.core.IndentedString

object GlobalStateManager {

  final class Value[A] private[GlobalStateManager] (val stateReference: StateReference, valueRef: AtomicReference[A]) {

    def get(): A =
      valueRef.get()

    def set(value: A): Unit =
      valueRef.set(value)

    def modify(f: A => A): A = {
      var loop = true
      var updated: A = null.asInstanceOf[A]
      while (loop) {
        val current = valueRef.get()
        updated = f(current)
        loop = !valueRef.compareAndSet(current, updated)
      }
      updated
    }

    def toIndentedString: IndentedString =
      IndentedString.section("Value:")(
        s"stateReference = $stateReference",
        s"value = ${valueRef.get()}",
      )

  }

  final class PageLocalState private[GlobalStateManager] (val pageReference: PageReference, stateRef: AtomicReference[Map[StateReference, Value[?]]]) {

    def getValue[A](stateReference: StateReference, initialIfDNE: => A): Value[A] =
      stateRef
        .get()
        .getOrElse(
          stateReference, {
            val value: Value[A] = Value(stateReference, new AtomicReference(initialIfDNE))
            var loop = true
            while (loop) {
              val current = stateRef.get()
              val updated = current.updated(stateReference, value)
              loop = !stateRef.compareAndSet(current, updated)
            }
            value
          },
        )
        .asInstanceOf[Value[A]]

    def toIndentedString: IndentedString =
      IndentedString.section("PageLocalState:")(
        s"pageReference = $pageReference",
        IndentedString.section("stateRef:")(mapToIndentedString(stateRef.get())(_.toIndentedString)),
      )

  }

  private val pageLocalRef: AtomicReference[Map[PageReference, PageLocalState]] = new AtomicReference(Map.empty)
  private val globalRef: AtomicReference[Map[StateReference, Value[?]]] = new AtomicReference(Map.empty)

  private[web] def getPageLocal(pageReference: PageReference): PageLocalState =
    pageLocalRef
      .get()
      .getOrElse(
        pageReference, {
          val pageData: PageLocalState = PageLocalState(pageReference, new AtomicReference(Map.empty))
          var loop = true
          while (loop) {
            val current = pageLocalRef.get()
            val updated = current.updated(pageReference, pageData)
            loop = !pageLocalRef.compareAndSet(current, updated)
          }
          pageData
        },
      )

  private[web] def getGlobal[A](stateReference: StateReference, initialIfDNE: => A): Value[A] =
    globalRef
      .get()
      .getOrElse(
        stateReference, {
          val value: Value[A] = Value(stateReference, new AtomicReference(initialIfDNE))
          var loop = true
          while (loop) {
            val current = globalRef.get()
            val updated = current.updated(stateReference, value)
            loop = !globalRef.compareAndSet(current, updated)
          }
          value
        },
      )
      .asInstanceOf[Value[A]]

  private[web] def releasePageLocal(pageReference: PageReference): Unit = {
    var loop = true
    while (loop) {
      val current = pageLocalRef.get()
      val updated = current.removed(pageReference)
      loop = !pageLocalRef.compareAndSet(current, updated)
    }
  }

  private def mapToIndentedString[K, A](map: Map[K, A])(f: A => IndentedString): IndentedString =
    map.toList.map { case (k, v) => IndentedString.section(k.toString)(f(v)) }

  def toIndentedString: IndentedString =
    IndentedString.section("GlobalStateManager:")(
      IndentedString.section("global:")(mapToIndentedString(globalRef.get())(_.toIndentedString)),
      IndentedString.section("stateLocal:")(mapToIndentedString(pageLocalRef.get())(_.toIndentedString)),
    )

}
