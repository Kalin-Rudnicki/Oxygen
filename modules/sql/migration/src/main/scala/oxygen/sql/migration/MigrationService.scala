package oxygen.sql.migration

import oxygen.core.Version
import oxygen.predef.core.*
import oxygen.predef.zio.*
import oxygen.sql.*
import oxygen.sql.migration.delta.MigrationPlanner
import oxygen.sql.migration.model.*
import oxygen.sql.migration.model.MigrationError.*
import oxygen.sql.migration.persistence.*
import oxygen.sql.migration.persistence.conversion.dbToDomain.*
import oxygen.sql.migration.persistence.conversion.domainToDb.*
import oxygen.sql.migration.persistence.model.PersistedMigrationFile
import oxygen.zio.system.Path
import zio.*

final class MigrationService(
    atomically: Atomically,
    repo: MigrationRepo,
    config: MigrationConfig,
) {

  //////////////////////////////////////////////////////////////////////////////////////////////////////
  //      API
  //////////////////////////////////////////////////////////////////////////////////////////////////////

  /** Load the migration files from the configured directory, then apply any that have not yet run. */
  def migrateUnverified: IO[MigrationError, MigrationResult] =
    loadAndApply(None)

  /**
    * Like [[migrateUnverified]], but first verifies (fail-fast) that the committed migrations reproduce the
    * given current-code schema -- catching the "forgot to regenerate a migration" mistake at
    * startup rather than later at query time.
    */
  def migrateVerified(verifyAgainst: MigrationSchema): IO[MigrationError, MigrationResult] =
    loadAndApply(verifyAgainst.some)

  /** Filesystem-agnostic core: apply already-loaded migration files. */
  def applyMigrations(migrations: ArraySeq[PersistedMigrationFile]): IO[MigrationError, MigrationResult] =
    applyMigrations(migrations, None)

  /** Like [[applyMigrations]], but first verifies the migrations reproduce the given current-code schema. */
  def applyMigrations(migrations: ArraySeq[PersistedMigrationFile], verifyAgainst: MigrationSchema): IO[MigrationError, MigrationResult] =
    applyMigrations(migrations, verifyAgainst.some)

  // TODO (KR) : support rollback

  //////////////////////////////////////////////////////////////////////////////////////////////////////
  //      Helpers
  //////////////////////////////////////////////////////////////////////////////////////////////////////

  private def loadAndApply(verifyAgainst: Option[MigrationSchema]): IO[MigrationError, MigrationResult] =
    for {
      dir <- Path.of(config.migrationsDir).mapError(e => ErrorReadingMigrationFiles(MigrationFs.MigrationFsError.FileSystemFailure(e)))
      entries <- MigrationFs(dir).list.mapError(ErrorReadingMigrationFiles(_))
      result <- applyMigrations(entries.map(_.file), verifyAgainst)
    } yield result

  private def applyMigrations(migrations: ArraySeq[PersistedMigrationFile], verifyAgainst: Option[MigrationSchema]): IO[MigrationError, MigrationResult] =
    for {
      _ <- ZIO.logDebug("Making sure oxygen migration is initialized")
      _ <- repo.initialize

      parsed <- ZIO.fromEither(parseAndOrder(migrations))

      _ <- ZIO.foreachDiscard(verifyAgainst)(verify(parsed, _))

      _ <- ZIO.logDebug("Loading executed migrations")
      executed <- repo.getMigrations.map(_.sortBy(_.version))

      toExecute <- ZIO.fromEither(reconcile(parsed, executed))

      _ <- ZIO.logInfo("No new migrations to run").whenDiscard(toExecute.isEmpty)
      newlyExecuted <- config.atomicity match {
        case MigrationConfig.Atomicity.None         => ZIO.foreach(toExecute)(execute)
        case MigrationConfig.Atomicity.PerMigration => ZIO.foreach(toExecute)(execute(_) @@ atomically)
        case MigrationConfig.Atomicity.AllOrNothing => ZIO.foreach(toExecute)(execute) @@ atomically
      }

      finalState = parsed.lastOption.fold(MigrationState.empty)(_._2.state.toDomain)
    } yield MigrationResult(finalState, newlyExecuted)

  /** Fail-fast if the latest filesystem snapshot does not reproduce the current-code schema. */
  private def verify(parsed: ArraySeq[(Version, PersistedMigrationFile)], schema: MigrationSchema): IO[MigrationError, Unit] = {
    val fsState = parsed.lastOption.fold(MigrationState.empty)(_._2.state.toDomain)
    ZIO
      .fromEither(MigrationService.pendingDiffs(fsState, schema))
      .mapError(ErrorVerifyingMigrations(_))
      .flatMap { pending => ZIO.fail(MigrationsStale(pending)).when(pending.nonEmpty).unit }
  }

  private def parseAndOrder(migrations: ArraySeq[PersistedMigrationFile]): Either[MigrationError, ArraySeq[(Version, PersistedMigrationFile)]] =
    migrations
      .traverse { file => Version.parse(file.version).toRight(InvalidMigrationVersion(file.version)).map((_, file)) }
      .map(_.sortBy(_._1))

  private def reconcile(
      parsed: ArraySeq[(Version, PersistedMigrationFile)],
      executed: ArraySeq[ExecutedMigration],
  ): Either[MigrationError, ArraySeq[(Version, PersistedMigrationFile)]] = {
    val byVersion: Map[Version, PersistedMigrationFile] = parsed.toMap
    val executedVersions: Set[Version] = executed.iterator.map(_.version).toSet
    val lastExecuted: Option[Version] = executed.map(_.version).maxOption

    for {
      // drift: every executed migration must exist on the filesystem AND match it
      _ <- executed.traverse { exe =>
        byVersion.get(exe.version) match
          case None       => MissingMigration(exe).asLeft
          case Some(file) => Either.cond(matches(exe, file), (), MigrationsDiffer(exe, file))
      }
      // new = filesystem migrations not yet executed; MVP requires them to come strictly after all executed (no out-of-order)
      toExecute = parsed.filterNot { case (v, _) => executedVersions.contains(v) }
      _ <- toExecute.traverse { case (v, _) =>
        lastExecuted match
          case Some(last) if Ordering[Version].lteq(v, last) => OutOfOrderMigration(v, last).asLeft
          case _                                             => ().asRight
      }
    } yield toExecute
  }

  /** An executed migration matches a filesystem file iff their diff steps are identical. */
  private def matches(executed: ExecutedMigration, file: PersistedMigrationFile): Boolean =
    executed.steps.length == file.diff.length &&
      executed.steps.iterator.zip(file.diff.iterator).forall { (e, f) =>
        e.stepNo == f.stepNo && (e.step match { case ExecutedMigration.StepType.Diff(d) => d.toDb == f.step })
      }

  private def execute(versioned: (Version, PersistedMigrationFile)): IO[StepError, ExecutedMigration] = {
    val (version, file) = versioned
    ZIO.withLogSpan(s"Migration $version") {
      for {
        startedAt <- Clock.instant
        _ <- ZIO.logInfo("Starting migration")
        _ <- repo.startMigration(version, startedAt)

        steps <- ZIO.foreach(file.diff)(repo.executeStep(version, _))

        completedAt <- Clock.instant
        _ <- ZIO.logInfo("Migration complete")
        _ <- repo.completeMigration(version, completedAt)
      } yield ExecutedMigration(version, steps, startedAt, completedAt.some)
    }
  }

}
object MigrationService {

  /////// Effects ///////////////////////////////////////////////////////////////

  def migrateUnverified: ZIO[MigrationService, MigrationError, MigrationResult] =
    ZIO.serviceWithZIO[MigrationService](_.migrateUnverified)

  def migrateVerified(verifyAgainst: MigrationSchema): ZIO[MigrationService, MigrationError, MigrationResult] =
    ZIO.serviceWithZIO[MigrationService](_.migrateVerified(verifyAgainst))

  def applyMigrations(migrations: ArraySeq[PersistedMigrationFile]): ZIO[MigrationService, MigrationError, MigrationResult] =
    ZIO.serviceWithZIO[MigrationService](_.applyMigrations(migrations))

  def applyMigrations(migrations: ArraySeq[PersistedMigrationFile], verifyAgainst: MigrationSchema): ZIO[MigrationService, MigrationError, MigrationResult] =
    ZIO.serviceWithZIO[MigrationService](_.applyMigrations(migrations, verifyAgainst))

  /////// Layers ///////////////////////////////////////////////////////////////

  val layer: URLayer[Atomically & MigrationRepo & MigrationConfig, MigrationService] =
    ZLayer.fromFunction { new MigrationService(_, _, _) }

  /**
    * You almost certainly want [[migrateUnverifiedLayer]]...
    *
    * The only reason to use this is if you want some special [[Atomically]]. Only real reason that would be the case would be for tests...
    */
  def customMigrateUnverifiedLayer: ZLayer[Database & Atomically & MigrationConfig, MigrationError, Database] =
    ZLayer {
      for {
        db <- ZIO.service[Database]
        atom <- ZIO.service[Atomically]
        config <- ZIO.service[MigrationConfig]
        repo = MigrationRepo.Live(db)
        service = MigrationService(atom, repo, config)
        _ <- service.migrateUnverified
      } yield db
    }

  /**
    * You almost certainly want [[migrateVerifiedLayer]]...
    *
    * The only reason to use this is if you want some special [[Atomically]].
    *
    * Only real reason that would be the case would be for tests... Even then, you should probably just be using ``
    */
  def customMigrateVerifiedLayer(verifyAgainst: MigrationSchema): ZLayer[Database & Atomically & MigrationConfig, MigrationError, Database] =
    ZLayer {
      for {
        db <- ZIO.service[Database]
        atom <- ZIO.service[Atomically]
        config <- ZIO.service[MigrationConfig]
        repo = MigrationRepo.Live(db)
        service = MigrationService(atom, repo, config)
        _ <- service.migrateVerified(verifyAgainst)
      } yield db
    }

  /**
    * It is recommended to use [[migrateVerifiedLayer]].
    *
    * Run a db migration using the FS as the source of truth. (Presumes that you want `Atomically.LiveDB` - aka: standard commit/rollback behavior).
    *
    * NO VERIFICATION against whether FS migration repr matches the current code.
    *
    * Usage: `Database.layer >>> MigrationService.migrateUnverifiedLayer`
    */
  def migrateUnverifiedLayer: ZLayer[Database & MigrationConfig, MigrationError, Database & Atomically] =
    Atomically.LiveDB.layer >+> customMigrateUnverifiedLayer

  /**
    * Run a db migration using the FS as the source of truth. (Presumes that you want `Atomically.LiveDB` - aka: standard commit/rollback behavior).
    *
    * RUNS VERIFICATION against whether FS migration repr matches the current code.
    *
    * Usage: `Database.layer >>> MigrationService.migrateVerifiedLayer(mySchema)`
    */
  def migrateVerifiedLayer(verifyAgainst: MigrationSchema): ZLayer[Database & MigrationConfig, MigrationError, Database & Atomically] =
    Atomically.LiveDB.layer >+> customMigrateVerifiedLayer(verifyAgainst)

  /////// Helpers ///////////////////////////////////////////////////////////////

  /**
    * Pure helper: the diffs still needed to turn the latest persisted filesystem state into the
    * current-code schema. Empty means the committed migrations are up to date with the code.
    */
  def pendingDiffs(latestFsState: MigrationState, schema: MigrationSchema): Either[StateDiffError, ArraySeq[StateDiff.Derivable]] =
    for {
      codeState <- MigrationState.fromTables(schema.tables)
      pending <- MigrationPlanner.diffStates(latestFsState, codeState)
    } yield pending

}
