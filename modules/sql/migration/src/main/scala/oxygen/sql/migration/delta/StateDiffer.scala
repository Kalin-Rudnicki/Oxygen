package oxygen.sql.migration.delta

import oxygen.predef.core.*
import oxygen.sql.migration.model.*
import oxygen.sql.migration.model.EntityRef.*
import oxygen.sql.migration.model.StateDiffError.*
import oxygen.sql.migration.model.StateDiffError.Cause.*

object StateDiffer {

  object phase1 {

    def diffs(
        current: MigrationState,
        target: MigrationState,
    ): Either[DiffError, Growable[StateDiff.CanBeDerived & StateDiff.DerivationPhase.Phase1]] =
      for {
        diffs1 <- schemaDiffs(current, target)
        diffs2 <- tableDiffs(current, target)
        diffs3 <- Growable.many(Ior.zippedMapIterator(current.tables, target.tables).map(_._2).toSeq).traverse {
          case Ior.Both(current, target) => columnDiffs(current, target)
          case Ior.Left(_)               => Growable.empty.asRight
          case Ior.Right(_)              => Growable.empty.asRight
        }
      } yield diffs1 ++ diffs2 ++ diffs3.flatten

    private def schemaDiffs(
        current: MigrationState,
        target: MigrationState,
    ): Either[DiffError, Growable[StateDiff.CanBeDerived & StateDiff.DerivationPhase.Phase1 & StateDiff.AlterSchema]] = {
      val createSchemas: Growable[StateDiff.AlterSchema.CreateSchema] =
        Growable.many(target.schemas &~ current.schemas).filterNot(_.isPublic).map { StateDiff.AlterSchema.CreateSchema(_) }
      val deleteSchemas: Growable[StateDiff.AlterSchema.DropSchema] =
        Growable.many(current.schemas &~ target.schemas).filterNot(_.isPublic).map { StateDiff.AlterSchema.DropSchema(_) }

      (createSchemas ++ deleteSchemas).asRight
    }

    private def tableDiffs(
        current: MigrationState,
        target: MigrationState,
    ): Either[DiffError, Growable[StateDiff.CanBeDerived & StateDiff.DerivationPhase.Phase1 & StateDiff.AlterTable]] =
      Growable
        .many(Ior.zippedMapIterator(current.tables, target.tables).map(_._2).toSeq)
        .traverse {
          case Ior.Both(_, _)    => Growable.empty.asRight
          case Ior.Left(current) => Growable.single(StateDiff.AlterTable.DropTable(current.tableName)).asRight
          case Ior.Right(target) => Growable.single(StateDiff.AlterTable.CreateTable(target)).asRight
        }
        .map(_.flatten)

    private def columnDiffs(
        current: TableState,
        target: TableState,
    ): Either[DiffError, Growable[StateDiff.CanBeDerived & StateDiff.DerivationPhase.Phase1 & StateDiff.AlterColumn]] =
      for {
        _ <- Either.cond(current.primaryKeyColumns.toSet == target.primaryKeyColumns.toSet, (), DiffError(current.tableName, InvalidPrimaryKeyAlteration))

        currentColMap = current.columns.map { c => (c.name, c) }.toMap
        targetColMap = target.columns.map { c => (c.name, c) }.toMap
        colDiffs <- Growable.many(Ior.zippedMapIterator(currentColMap, targetColMap).map(_._2).toSeq).traverse {
          case Ior.Both(currentCol, targetCol) =>
            def colRef: ColumnRef = ColumnRef(current.tableName, currentCol.name)
            if currentCol.columnType != targetCol.columnType then DiffError(colRef, SameNameDifferentType).asLeft
            else if currentCol.nullable != targetCol.nullable then Growable.single(StateDiff.AlterColumn.SetNullable(colRef, targetCol.nullable)).asRight
            else Growable.empty.asRight
          case Ior.Left(currentCol) => Growable.single(StateDiff.AlterColumn.DropColumn(ColumnRef(current.tableName, currentCol.name))).asRight
          case Ior.Right(targetCol) => Growable.single(StateDiff.AlterColumn.CreateColumn(current.tableName, targetCol)).asRight
        }
      } yield colDiffs.flatten

  }

  object phase2 {

    def diffs(
        current: MigrationState,
        target: MigrationState,
    ): Either[DiffError, Growable[StateDiff.CanBeDerived & StateDiff.DerivationPhase.Phase2]] =
      for {
        diffs1 <- Growable.many(Ior.zippedMapIterator(current.tables, target.tables).map(_._2).toSeq).traverse {
          case Ior.Both(current, target) =>
            for {
              a <- foreignKeyDiffs(current, target)
              b <- indexDiffs(current, target)
            } yield a ++ b
          case Ior.Left(_)  => Growable.empty.asRight
          case Ior.Right(_) => Growable.empty.asRight
        }
      } yield diffs1.flatten

    private def foreignKeyDiffs(
        current: TableState,
        target: TableState,
    ): Either[DiffError, Growable[StateDiff.CanBeDerived & StateDiff.DerivationPhase.Phase2 & StateDiff.AlterForeignKey]] = {
      val currentFKMap = current.foreignKeys.map { fk => (fk.fkName, fk) }.toMap
      val targetFKMap = target.foreignKeys.map { fk => (fk.fkName, fk) }.toMap

      Growable
        .many(Ior.zippedMapIterator(currentFKMap, targetFKMap).map(_._2).toSeq)
        .traverse {
          case Ior.Both(currentFK, targetFK) =>
            if currentFK == targetFK then Growable.empty.asRight
            else DiffError(currentFK.ref, InvalidForeignKeyAlteration).asLeft
          case Ior.Left(currentFK) =>
            Growable.single(StateDiff.AlterForeignKey.DropForeignKey(currentFK.ref)).asRight
          case Ior.Right(targetFK) =>
            Growable.single(StateDiff.AlterForeignKey.CreateForeignKey(targetFK)).asRight
        }
        .map(_.flatten)
    }

    private def indexDiffs(
        current: TableState,
        target: TableState,
    ): Either[DiffError, Growable[StateDiff.CanBeDerived & StateDiff.DerivationPhase.Phase2 & StateDiff.AlterIndex]] = {
      val currentIdxMap = current.indices.map { idx => (idx.idxName, idx) }.toMap
      val targetIdxMap = target.indices.map { idx => (idx.idxName, idx) }.toMap

      Growable
        .many(Ior.zippedMapIterator(currentIdxMap, targetIdxMap).map(_._2).toSeq)
        .traverse {
          case Ior.Both(currentIdx, targetIdx) =>
            if currentIdx == targetIdx then Growable.empty.asRight
            else DiffError(currentIdx.ref, InvalidIndexAlteration).asLeft
          case Ior.Left(currentIdx) =>
            Growable.single(StateDiff.AlterIndex.DropIndex(currentIdx.ref)).asRight
          case Ior.Right(targetIdx) =>
            Growable.single(StateDiff.AlterIndex.CreateIndex(targetIdx)).asRight
        }
        .map(_.flatten)
    }

  }

  object phase3 {

    def diffs(
        current: MigrationState,
        specified: StateDiff.CanOnlyBeSpecified,
    ): Either[DiffError, Growable[StateDiff.CanBeDerived & StateDiff.DerivationPhase.Phase3]] =
      for {
        diffs1 <- Growable.many(current.tables.values).traverse(foreignKeyDiffs(_, specified))
        diffs2 <- Growable.many(current.tables.values).traverse(indexDiffs(_, specified))
      } yield diffs1.flatten ++ diffs2.flatten

    private def foreignKeyDiffs(
        current: TableState,
        specified: StateDiff.CanOnlyBeSpecified,
    ): Either[DiffError, Growable[StateDiff.CanBeDerived & StateDiff.DerivationPhase.Phase3 & StateDiff.AlterForeignKey]] =
      Growable.many(current.foreignKeys).traverse(foreignKeyDiffs(_, specified)).map(_.flatten)

    private def foreignKeyDiffs(
        current: ForeignKeyState,
        specified: StateDiff.CanOnlyBeSpecified,
    ): Either[DiffError, Growable[StateDiff.CanBeDerived & StateDiff.DerivationPhase.Phase3 & StateDiff.AlterForeignKey]] =
      if current.explicitFKName.isEmpty then {
        val newAutoRef: ForeignKeyState.AutoRef =
          specified match {
            case StateDiff.AlterSchema.RenameSchema(schemaRef, newName)       => current.renameSchema(schemaRef, newName).autoRef
            case StateDiff.AlterTable.RenameTable(tableRef, newName)          => current.renameTable(tableRef, newName).autoRef
            case StateDiff.AlterColumn.RenameColumn(columnRef, newName)       => current.renameColumn(columnRef, newName).autoRef
            case _: StateDiff.AlterForeignKey.RenameExplicitlyNamedForeignKey => current.autoRef
            case _: StateDiff.AlterIndex.RenameExplicitlyNamedIndex           => current.autoRef
          }

        if newAutoRef == current.autoRef then Growable.empty.asRight
        else Growable.single(StateDiff.AlterForeignKey.RenameAutoNamedForeignKey(current.ref, newAutoRef.autoFKName)).asRight
      } else Growable.empty.asRight

    private def indexDiffs(
        current: TableState,
        specified: StateDiff.CanOnlyBeSpecified,
    ): Either[DiffError, Growable[StateDiff.CanBeDerived & StateDiff.DerivationPhase.Phase3 & StateDiff.AlterIndex]] =
      Growable.many(current.indices).traverse(indexDiffs(_, specified)).map(_.flatten)

    private def indexDiffs(
        current: IndexState,
        specified: StateDiff.CanOnlyBeSpecified,
    ): Either[DiffError, Growable[StateDiff.CanBeDerived & StateDiff.DerivationPhase.Phase3 & StateDiff.AlterIndex]] =
      if current.explicitIdxName.isEmpty then {
        val newAutoRef: IndexState.AutoRef =
          specified match {
            case StateDiff.AlterSchema.RenameSchema(schemaRef, newName)       => current.renameSchema(schemaRef, newName).autoRef
            case StateDiff.AlterTable.RenameTable(tableRef, newName)          => current.renameTable(tableRef, newName).autoRef
            case StateDiff.AlterColumn.RenameColumn(columnRef, newName)       => current.renameColumn(columnRef, newName).autoRef
            case _: StateDiff.AlterForeignKey.RenameExplicitlyNamedForeignKey => current.autoRef
            case _: StateDiff.AlterIndex.RenameExplicitlyNamedIndex           => current.autoRef
          }

        if newAutoRef == current.autoRef then Growable.empty.asRight
        else Growable.single(StateDiff.AlterIndex.RenameAutoNamedIndex(current.ref, newAutoRef.autoIdxName)).asRight
      } else Growable.empty.asRight

  }

}
