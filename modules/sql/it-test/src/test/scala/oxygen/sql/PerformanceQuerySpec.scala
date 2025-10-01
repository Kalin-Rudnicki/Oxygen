package oxygen.sql

import oxygen.core.syntax.number.*
import oxygen.predef.test.{*, given}
import oxygen.sql.error.QueryError
import oxygen.sql.migration.*
import oxygen.sql.migration.model.*
import oxygen.sql.migration.persistence.MigrationRepo
import scala.annotation.experimental
import zio.*
import zio.stream.*

/**
  * local psql container, -Xmx6G, usePar = 4, insertGroupSize* = 1_000, insertNumGroups* = 100, total = 100_000
  * |-------------------------------------|----------|------------------|----------------|------------|
  * |                Name                 | Num Rows | Duration (total) | Duration (avg) | Rows / Min |
  * |-------------------------------------|----------|------------------|----------------|------------|
  * | sequential(sequential(non-batched)) |  100,000 |         2 m 53 s |           1 ms |     34,627 |
  * | parallel(sequential(non-batched))   |  100,000 |      55 s 170 ms |      551702 ns |    108,754 |
  * | sequential(batched)                 |  100,000 |       3 s 295 ms |       32950 ns |  1,820,940 |
  * | parallel(batched)                   |  100,000 |           738 ms |        7383 ns |  8,126,777 |
  * | batched                             |  100,000 |       2 s 610 ms |       26108 ns |  2,298,146 |
  * | batchOptimized - sequential         |  100,000 |           871 ms |        8718 ns |  6,882,312 |
  * | batchOptimized - parallel           |  100,000 |           280 ms |        2807 ns | 21,375,133 |
  * |-------------------------------------|----------|------------------|----------------|------------|
  *
  * local psql container, -Xmx6G, usePar = 16, insertGroupSize* = 1_000, insertNumGroups* = 100, total = 100_000
  * |-------------------------------------|----------|------------------|----------------|------------|
  * |                Name                 | Num Rows | Duration (total) | Duration (avg) | Rows / Min |
  * |-------------------------------------|----------|------------------|----------------|------------|
  * | sequential(sequential(non-batched)) |  100,000 |         2 m 17 s |           1 ms |     43,519 |
  * | parallel(sequential(non-batched))   |  100,000 |       9 s 793 ms |       97936 ns |    612,644 |
  * | sequential(batched)                 |  100,000 |       1 s 335 ms |       13351 ns |  4,494,045 |
  * | parallel(batched)                   |  100,000 |           219 ms |        2193 ns | 27,359,781 |
  * | batched                             |  100,000 |       1 s 160 ms |       11603 ns |  5,171,076 |
  * | batchOptimized - sequential         |  100,000 |           778 ms |        7786 ns |  7,706,139 |
  * | batchOptimized - parallel           |  100,000 |           209 ms |        2091 ns | 28,694,404 |
  * |-------------------------------------|----------|------------------|----------------|------------|
  *
  * local psql container, -Xmx6G, usePar = 16, insertGroupSize* = 1_000, insertNumGroups* = 1_000, total = 1_000_000
  * |-----------------------------|-----------|------------------|----------------|------------|
  * |            Name             | Num Rows  | Duration (total) | Duration (avg) | Rows / Min |
  * |-----------------------------|-----------|------------------|----------------|------------|
  * | sequential(batched)         | 1,000,000 |      17 s 651 ms |       17651 ns |  3,399,240 |
  * | parallel(batched)           | 1,000,000 |       2 s 237 ms |        2237 ns | 26,821,636 |
  * | batchOptimized - sequential | 1,000,000 |       6 s 143 ms |        6143 ns |  9,767,214 |
  * | batchOptimized - parallel   | 1,000,000 |       1 s 109 ms |        1109 ns | 54,102,795 |
  * |-----------------------------|-----------|------------------|----------------|------------|
  *
  * local psql container, -Xmx6G, usePar = 16, insertGroupSize* = 1_000, insertNumGroups* = 10_000, total = 10_000_000
  * |-----------------------------|------------|------------------|----------------|------------|
  * |            Name             |  Num Rows  | Duration (total) | Duration (avg) | Rows / Min |
  * |-----------------------------|------------|------------------|----------------|------------|
  * | sequential(batched)         | 10,000,000 |         3 m 23 s |       20338 ns |  2,950,142 |
  * | parallel(batched)           | 10,000,000 |      26 s 724 ms |        2672 ns | 22,455,089 |
  * | batchOptimized - sequential | 10,000,000 |         1 m 21 s |        8171 ns |  7,343,042 |
  * | batchOptimized - parallel   | 10,000,000 |      21 s 583 ms |        2158 ns | 27,803,521 |
  * |-----------------------------|------------|------------------|----------------|------------|
  */
@experimental
object PerformanceQuerySpec extends OxygenSpec[Database] {

  // override def defaultLogLevel: LogLevel = LogLevel.Trace

  private object env {
    def getInt(n: String): Int = java.lang.System.getenv(n).toInt
    def getInts(n: String): Chunk[Int] = Chunk.from(java.lang.System.getenv(n).split(',')).filter(_.nonEmpty).map(_.toInt)
    def getBoolean(n: String): Boolean = java.lang.System.getenv(n).toBoolean
  }

  private lazy val pars: Chunk[Int] = env.getInts("QPERF_PARS")
  private lazy val minEfficiency: Int = env.getInt("QPERF_MIN_EFFICIENCY")

  private lazy val batchSize: Int = env.getInt("QPERF_BATCH_SIZE")
  private lazy val numBatches: Int = env.getInt("QPERF_NUM_BATCHES")

  final case class PerfCase(
      eff: Int,
      name: String,
      exec: ZStream[Database, QueryError, Person] => ZIO[Database, QueryError, Long],
  ) {

    def execute: ZIO[Database, QueryError, PerfCase.Executed] =
      ZIO.logInfo(s"Running : $name") *>
        Person.truncateCascade().unit *>
        exec(cacheStream).timed
          .map { case (duration, size) => PerfCase.Executed(name, size, duration) }
          .tap { v => ZIO.logInfo(s"Completed ($name) in ${v.renderedTotalDuration}") }

    def executeOption: ZIO[Database, QueryError, Option[PerfCase.Executed]] =
      execute.when(eff >= minEfficiency)

  }
  object PerfCase {

    final case class Executed(name: String, size: Long, totalDuration: Duration) {
      val renderedTotalDuration: String = totalDuration.render

      val avgDuration: Duration = totalDuration.dividedBy(size)
      val renderedAvgDuration: String = avgDuration.render
      val numRowsString: String = size.toStringCommas
      val rowsPerMinute: Long = 1.minute.dividedBy(avgDuration)
      val rowsPerMinuteString: String = rowsPerMinute.toStringCommas

      def showRow(nameWidth: Int, numRowsWidth: Int, totalDurationWidth: Int, avgDurationWidth: Int, rowsPerMinuteWidth: Int): String =
        s"| ${name.alignLeft(nameWidth)} | ${numRowsString.alignRight(numRowsWidth)} | ${renderedTotalDuration.alignRight(totalDurationWidth)} | ${renderedAvgDuration.alignRight(avgDurationWidth)} | ${rowsPerMinuteString.alignRight(rowsPerMinuteWidth)} |"

    }

    ///////  ///////////////////////////////////////////////////////////////

    private def make(eff: Int, name: String)(exec: ZStream[Database, QueryError, Person] => ZIO[Database, QueryError, Long]): PerfCase =
      PerfCase(eff, name, exec)

    lazy val _1: PerfCase =
      make(1, "[jdbcBatch = N/A, aggValues = false, par = N/A]") { _.mapZIO { Person.insert(_).updated.map(_.toLong) }.runSum }

    lazy val _2: Chunk[PerfCase] =
      for {
        p <- pars
      } yield make(1, s"[jdbcBatch = N/A, aggValues = false, par = $p]") { _.mapZIOParUnordered(p) { Person.insert(_).updated.map(_.toLong) }.runSum }

    lazy val _3: Chunk[PerfCase] =
      for {
        b <- Chunk(1_000, 5_000, 25_000)
      } yield make(2, s"[jdbcBatch = $b, aggValues = false, par = N/A]") { _.rechunk(b).chunks.mapZIO { Person.insert.batched(_).updated.map(_.map(_.toLong).sum) }.runSum }

    lazy val _4: Chunk[PerfCase] =
      for {
        p <- pars
        b <- Chunk(1_000, 5_000, 25_000)
      } yield make(2, s"[jdbcBatch = $b, aggValues = false, par = $p]") { _.rechunk(b).chunks.mapZIOParUnordered(p) { Person.insert.batched(_).updated.map(_.map(_.toLong).sum) }.runSum }

    lazy val _5: PerfCase =
      make(3, "[jdbcBatch = N/A, aggValues = true, par = N/A]") { Person.batchOptimizedInsert.insert.stream(_, _.sequential) }

    lazy val _6: Chunk[PerfCase] =
      for {
        p <- pars
      } yield make(3, s"[jdbcBatch = N/A, aggValues = true, par = $p]") { Person.batchOptimizedInsert.insert.stream(_, _.parallel(p)) }

    lazy val _7: Chunk[PerfCase] =
      for {
        b <- Chunk(4, 16, 64)
      } yield make(4, s"[jdbcBatch = $b, aggValues = true, par = N/A]") { Person.batchOptimizedInsert.insert.stream(_, _.sequential.withAdditionalJdbcBatchSize(b)) }

    lazy val _8: Chunk[PerfCase] =
      for {
        p <- pars
        b <- Chunk(4, 16, 64)
      } yield make(4, s"[jdbcBatch = $b, aggValues = true, par = $p]") { Person.batchOptimizedInsert.insert.stream(_, _.parallel(p).withAdditionalJdbcBatchSize(b)) }

    lazy val allCases: Chunk[PerfCase] =
      Chunk(
        Chunk(_1),
        _2,
        _3,
        _4,
        Chunk(_5),
        _6,
        _7,
        _8,
      ).flatten

  }

  private object genData {

    private val genChunk: ZChannel[Any, Any, Any, Any, Nothing, Chunk[PersonCache], Unit] =
      ZChannel.fromZIO { PersonCache.generate().replicateZIO(batchSize).map(Chunk.from) }.flatMap { ZChannel.write(_) }

    private def genChunks(n: Int): ZChannel[Any, Any, Any, Any, Nothing, Chunk[PersonCache], Unit] =
      if (n < numBatches) genChunk *> genChunks(n + 1)
      else ZChannel.unit

    val stream: UStream[PersonCache] = ZStream.fromChannel(genChunks(0))

  }

  private val cacheStream: ZStream[Database, QueryError, Person] =
    for {
      groupId <- ZStream.fromZIO { Random.nextUUID }
      pc <- PersonCache.selectAll.execute().stream
    } yield pc.toPerson(groupId)

  override def testSpec: TestSpec =
    suite("query performance")(
      test("inserts") {
        for {
          _ <- ZIO.logInfo("Running db performance spec")

          _ <- ZIO.logInfo("Generating data and inserting into cache table...")
          (dur, _) <- PersonCache.batchOptimizedInsert.insert.stream(genData.stream, _.parallel(8)).timed
          _ <- ZIO.logInfo(s"Completed data generation and insert (${(batchSize * numBatches).toStringCommas}) : ${dur.render}")
          cacheRows <- PersonCache.select_*.execute().single
          _ <- ZIO.logInfo(s"Num rows in cache table: ${cacheRows.toStringCommas}")

          allResults <- ZIO.foreach(PerfCase.allCases)(_.executeOption).map(_.flatten)

          // =====| Data |=====

          nameColHeader = "Name"
          numRowsHeader = "Num Rows"
          totalDurationHeader = "Duration (total)"
          avgDurationHeader = "Duration (avg)"
          rowsPerMinuteHeader = "Rows / Min"

          maxNameLen = allResults.map(_.name.length).max.max(nameColHeader.length)
          maxNumRowsLen = allResults.map(_.numRowsString.length).max.max(numRowsHeader.length)
          maxTotalLen = allResults.map(_.renderedTotalDuration.length).max.max(totalDurationHeader.length)
          maxAvgLen = allResults.map(_.renderedAvgDuration.length).max.max(avgDurationHeader.length)
          maxRowsPerMinuteLen = allResults.map(_.rowsPerMinuteString.length).max.max(rowsPerMinuteHeader.length)

          headerRow =
            s"| ${nameColHeader.alignCenter(maxNameLen)} | ${numRowsHeader.alignCenter(maxNumRowsLen)} | ${totalDurationHeader.alignCenter(maxTotalLen)} | ${avgDurationHeader.alignCenter(maxAvgLen)} | ${rowsPerMinuteHeader.alignCenter(maxRowsPerMinuteLen)} |"
          sepRow = s"|${"-" * (maxNameLen + 2)}|${"-" * (maxNumRowsLen + 2)}|${"-" * (maxTotalLen + 2)}|${"-" * (maxAvgLen + 2)}|${"-" * (maxRowsPerMinuteLen + 2)}|"

          allRows =
            Chunk(
              Chunk(sepRow, headerRow, sepRow),
              allResults.map(_.showRow(maxNameLen, maxNumRowsLen, maxTotalLen, maxAvgLen, maxRowsPerMinuteLen)),
              Chunk(sepRow),
            ).flatten

          _ <- ZIO.logImportant(allRows.mkString("\n"))
        } yield assertCompletes
      },
    ) @@ OxygenAspects.withMinLogLevel.detailed @@ TestAspect.tag("performance")

  override def testAspects: Chunk[PerformanceQuerySpec.TestSpecAspect] = Chunk(TestAspect.nondeterministic, TestAspect.withLiveClock)

  override def layerProvider: LayerProvider[R] =
    LayerProvider.provideShared[Env](
      Helpers.testContainerLayer,
      Helpers.databaseLayer,
      MigrationService.layer,
      MigrationConfig.defaultLayer,
      MigrationService.migrateLayer(
        Migrations(
          PlannedMigration.auto(1)(Person.tableRepr, PersonCache.tableRepr),
        ),
      ),
      Atomically.LiveDB.layer,
      MigrationRepo.layer,
    )

}
