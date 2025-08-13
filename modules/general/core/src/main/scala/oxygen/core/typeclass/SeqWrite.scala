package oxygen.core.typeclass

import scala.collection.mutable

trait SeqWrite[F[_]] {

  def newBuilder[A]: mutable.Builder[A, F[A]]

  final def fromIterableOnce[A](i: IterableOnce[A]): F[A] = {
    val builder = newBuilder[A]
    builder.sizeHint(i.knownSize)
    builder.addAll(i)
    builder.result()
  }
}
object SeqWrite {

  given fromSeqOps: [S[_]: SeqOps as seqOps] => SeqWrite[S] = seqOps

}
