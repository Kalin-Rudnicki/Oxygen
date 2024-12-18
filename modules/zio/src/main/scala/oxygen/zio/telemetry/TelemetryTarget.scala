package oxygen.zio.telemetry

import oxygen.json.KeyedMapDecoder
import oxygen.predef.color.{*, given}
import oxygen.predef.core.*
import oxygen.zio.logger.*
import zio.{LogLevel as _, *}
import zio.json.JsonDecoder

trait TelemetryTarget {

  val name: String
  val handle: TraceEntry => UIO[Unit]

  override def toString: String = s"TraceTarget[$name]"

}
object TelemetryTarget {

  final class InMemory private (
      private val tracesRef: Ref[Map[String, Chunk[TraceEntry]]],
  ) extends TelemetryTarget {

    override val name: String = "in-memory"

    override val handle: TraceEntry => UIO[Unit] =
      trace =>
        tracesRef.update {
          _.updatedWith(trace.key) {
            case Some(existing) => (existing :+ trace).some
            case None           => Chunk.single(trace).some
          }
        }

  }
  object InMemory {

    // TODO (KR) : make this shared once there are other TraceTargets
    // TODO (KR) : add the ability to specify bucket ranges
    final case class TraceAggregator(
        key: String,
        segregateByArgs: Set[String],
        pollDuration: Duration,
        aggregationDuration: Duration,
        someLevel: LogLevel,
        noneLevel: Option[LogLevel],
    ) derives JsonDecoder

    final case class Stats(
        count: Int,
        minMS: Long,
        maxMS: Long,
        avgMS: Long,
    ) {
      def show: String =
        Seq(
          "count" -> count.toString,
          "avg" -> avgMS.millis.render,
          "min" -> minMS.millis.render,
          "max" -> maxMS.millis.render,
        ).map { case (k, v) => color"${k.blueFg}=${v.yellowFg}" }
          .csMkString(", ")
          .toString
    }
    object Stats {

      def calculate(durations: Chunk[Long]): Stats =
        Stats(
          count = durations.length,
          minMS = durations.min,
          maxMS = durations.max,
          avgMS = durations.sum / durations.length,
        )

    }

    private def runAggregationFiber(
        inMemory: InMemory,
        aggregator: TraceAggregator,
    ): URIO[Scope, Unit] =
      (for {
        _ <- Logger.log.trace(s"Running in-memory telemetry report for '${aggregator.key}'")
        now <- Clock.instant
        considerAfter = now.minus(aggregator.aggregationDuration)
        traceMap <- inMemory.tracesRef.get
        traces = traceMap.getOrElse(aggregator.key, Chunk.empty).filter(_.end.isAfter(considerAfter))
        _ <-
          if (traces.isEmpty)
            ZIO.foreachDiscard(aggregator.noneLevel)(Logger.log(_)(s"No traces for '${aggregator.key}'"))
          else {
            val tmp1 =
              traces.groupMap { t =>
                Chunk.fromIterable(aggregator.segregateByArgs.map(a => a -> t.args.getOrElse(a, "<missing>"))).sorted :+ ("<outcome>" -> t.outcome.toString)
              }(_.duration.toMillis)
            val tmp2 =
              ("all" -> Stats.calculate(traces.map(_.duration.toMillis))) +:
                tmp1.toSeq
                  .map { case (k, durationMS) =>
                    k.map { case (k2, v2) => color"${k2.cyanFg}=${v2.magentaFg}" }.csMkString(", ").toString ->
                      Stats.calculate(durationMS)
                  }
                  .sortBy(_._2.avgMS)
                  .reverse
            val tmp3 =
              tmp2.map { case (k, v) =>
                s"\n  $k : ${v.show}"
              }
            Logger.log(aggregator.someLevel)(
              s"Telemetry Report [${aggregator.key}]${tmp3.mkString}",
            )
          }
      } yield ()).repeat(Schedule.fixed(aggregator.pollDuration)).forkScoped.unit

    private def runCleanupFiber(
        inMemory: InMemory,
        duration: Duration,
    ): URIO[Scope, Unit] =
      (for {
        _ <- Logger.log.trace(s"Running in-memory telemetry cleanup")
        now <- Clock.instant
        removeBefore = now.minus(duration)
        _ <- inMemory.tracesRef.update {
          _.map { case (k, v) => k -> v.filter(_.end.isAfter(removeBefore)) }.filter(_._2.nonEmpty)
        }
      } yield ()).repeat(Schedule.fixed(duration)).forkScoped.unit

    // TODO (KR) : support an option for logging unspecified keys
    def make(aggregators0: TraceAggregator, aggregatorsN: TraceAggregator*): URIO[Scope, TelemetryTarget] =
      for {
        tracesRef <- Ref.make(Map.empty[String, Chunk[TraceEntry]])
        inMemory = InMemory(tracesRef)

        aggregators = aggregators0 +: aggregatorsN

        _ <- ZIO.foreachDiscard(aggregators)(runAggregationFiber(inMemory, _))
        _ <- runCleanupFiber(inMemory, aggregators.map(_.aggregationDuration).max.dividedBy(3))
      } yield inMemory

  }

  final case class ConfigBuilder(telemetryTarget: RIO[Scope, Chunk[TelemetryTarget]])
  object ConfigBuilder {

    val inMemory: KeyedMapDecoder.Decoder[ConfigBuilder] =
      KeyedMapDecoder.Decoder.make[NonEmptyChunk[InMemory.TraceAggregator]]("inMemory").map { aggs =>
        ConfigBuilder(InMemory.make(aggs.head, aggs.tail*).map(Chunk.single))
      }

    // =====|  |=====

    val default: Seq[KeyedMapDecoder.Decoder[ConfigBuilder]] =
      Seq(inMemory)

  }

}
