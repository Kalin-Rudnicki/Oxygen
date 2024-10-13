package oxygen.core.syntax

import scala.collection.IterableOps

object seq {

  extension [F[_A] <: IterableOps[_A, F, F[_A]], A](self: F[A]) {

    def mapPassLeft[B, C](passInit: B)(f: (B, A) => (B, C)): (F[C], B) = {
      var pass: B = passInit
      val iterator = self
      val builder = self.iterableFactory.newBuilder[C]

      iterator.foreach { e =>
        val (tmpPass, c) = f(pass, e)
        pass = tmpPass
        builder.addOne(c)
      }

      (builder.result(), pass)
    }

    def intersperse[B >: A](join: B): F[B] = {
      val builder = self.iterableFactory.newBuilder[B]
      val iter = self.iterator

      if (iter.hasNext) {
        builder.addOne(iter.next())
        while (iter.hasNext) {
          builder.addOne(join)
          builder.addOne(iter.next())
        }
      }

      builder.result()
    }

    def surround[B >: A](start: B, join: B, end: B): F[B] = {
      val builder = self.iterableFactory.newBuilder[B]
      val iter = self.iterator

      builder.addOne(start)
      if (iter.hasNext) {
        builder.addOne(iter.next())
        while (iter.hasNext) {
          builder.addOne(join)
          builder.addOne(iter.next())
        }
      }
      builder.addOne(end)

      builder.result()
    }

    def surround[B >: A](startJoinEnd: B): F[B] =
      surround(startJoinEnd, startJoinEnd, startJoinEnd)

  }

}
