package oxygen.zio.syntax

import oxygen.zio.SparseStreamAggregator
import zio.stream.*

object stream {

  extension [R, E, A](self: ZStream[R, E, A])
    def agg[B](streamAggregator: SparseStreamAggregator[A, B]): ZStream[R, E, B] =
      streamAggregator.aggregateStream(self)

}
