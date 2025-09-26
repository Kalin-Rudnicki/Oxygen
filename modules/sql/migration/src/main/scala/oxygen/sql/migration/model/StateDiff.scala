package oxygen.sql.migration.model

import oxygen.predef.core.*
import oxygen.sql.migration.model.EntityRef.*
import oxygen.sql.migration.model.StateDiff.*
import oxygen.sql.schema.*

sealed trait StateDiff { self: StateDiff.CoreType =>

  final lazy val applicationOrder: Int = this match
    case _: AlterSchema.CreateSchema                        => 1
    case _: AlterSchema.RenameSchema                        => 2
    case _: AlterTable.CreateTable                          => 3
    case _: AlterTable.RenameTable                          => 4
    case _: AlterColumn.CreateColumn                        => 5
    case _: AlterColumn.RenameColumn                        => 6
    case _: AlterForeignKey.DropForeignKey                  => 7
    case _: AlterForeignKey.RenameExplicitlyNamedForeignKey => 8
    case _: AlterForeignKey.RenameAutoNamedForeignKey       => 9
    case _: AlterForeignKey.CreateForeignKey                => 10
    case _: AlterColumn.SetNullable                         => 11
    case _: AlterColumn.DropColumn                          => 12
    case _: AlterTable.DropTable                            => 13
    case _: AlterSchema.DropSchema                          => 14

  // TODO (KR) : improve
  final def toIndentedString: IndentedString = this.toString

  def specifiableUnion: StateDiff.Specifiable.Union
  def derivableUnion: StateDiff.Derivable.Union
  def coreUnion: StateDiff.CoreType.Union

}
object StateDiff {

  //////////////////////////////////////////////////////////////////////////////////////////////////////
  //      Specifiable / Derivable
  //////////////////////////////////////////////////////////////////////////////////////////////////////

  sealed trait CanDoSomething { self: Specifiable.CanBeSpecified | Derivable.CanBeDerived => }

  sealed trait Specifiable private () extends StateDiff { self: CoreType & Specifiable.Union =>
    val canBeSpecified: Boolean
    override final def specifiableUnion: Specifiable.Union = self
  }
  object Specifiable {
    type Union = CanBeSpecified | CanNotBeSpecified

    sealed trait CanBeSpecified extends Specifiable, CanDoSomething { self: CanBeSpecifiedAndDerived | CanOnlyBeSpecified =>
      override final val canBeSpecified: Boolean = true
    }

    sealed trait CanNotBeSpecified extends Specifiable { self: CanOnlyBeDerived =>
      override final val canBeSpecified: Boolean = false
    }

  }
  export Specifiable.{CanBeSpecified, CanNotBeSpecified}

  sealed trait Derivable private () extends StateDiff { self: CoreType & Derivable.Union =>
    val canBeDerived: Boolean
    override final def derivableUnion: Derivable.Union = self
  }
  object Derivable {
    type Union = CanBeDerived | CanNotBeDerived

    sealed trait CanBeDerived extends Derivable, CanDoSomething { self: (CanBeSpecifiedAndDerived | CanOnlyBeDerived) & DerivationPhase =>
      override final val canBeDerived: Boolean = true
    }

    sealed trait CanNotBeDerived extends Derivable { self: CanOnlyBeSpecified =>
      override final val canBeDerived: Boolean = false
    }

  }
  export Derivable.{CanBeDerived, CanNotBeDerived}

  //////////////////////////////////////////////////////////////////////////////////////////////////////
  //      CoreType
  //////////////////////////////////////////////////////////////////////////////////////////////////////

  sealed trait CoreType private () extends StateDiff { self: CanDoSomething & CoreType.Union =>
    override final def coreUnion: CoreType.Union = self
  }
  object CoreType {
    type Union = CanBeSpecifiedAndDerived | CanOnlyBeSpecified | CanOnlyBeDerived

    sealed trait CanBeSpecifiedAndDerived extends CoreType, CanBeSpecified, CanBeDerived { self: DerivationPhase => }
    sealed trait CanOnlyBeSpecified extends CoreType, CanBeSpecified, CanNotBeDerived
    sealed trait CanOnlyBeDerived extends CoreType, CanNotBeSpecified, CanBeDerived { self: DerivationPhase => }
  }
  export CoreType.{CanBeSpecifiedAndDerived, CanOnlyBeDerived, CanOnlyBeSpecified}

  //////////////////////////////////////////////////////////////////////////////////////////////////////
  //      Phases
  //////////////////////////////////////////////////////////////////////////////////////////////////////

  sealed trait DerivationPhase private () { self: CanBeDerived =>
    val derivationPhase: Int
  }
  object DerivationPhase {

    sealed trait Phase1 extends DerivationPhase { self: CanBeDerived =>
      override final val derivationPhase: Int = 1
    }

    sealed trait Phase2 extends DerivationPhase { self: CanBeDerived =>
      override final val derivationPhase: Int = 2
    }

    sealed trait Phase3 extends DerivationPhase { self: CanBeDerived =>
      override final val derivationPhase: Int = 3
    }

  }

  //////////////////////////////////////////////////////////////////////////////////////////////////////
  //      Leaves
  //////////////////////////////////////////////////////////////////////////////////////////////////////

  sealed trait AlterSchema extends StateDiff { self: CoreType =>
    val schemaRef: SchemaRef
  }
  object AlterSchema {

    final case class CreateSchema(schemaRef: SchemaRef) extends AlterSchema, CanBeSpecifiedAndDerived, DerivationPhase.Phase1

    final case class RenameSchema(schemaRef: SchemaRef, newName: String) extends AlterSchema, CanOnlyBeSpecified

    final case class DropSchema(schemaRef: SchemaRef) extends AlterSchema, CanBeSpecifiedAndDerived, DerivationPhase.Phase1

  }

  sealed trait AlterTable extends StateDiff { self: CoreType =>
    val tableRef: TableRef
  }
  object AlterTable {

    final case class CreateTable(table: TableState) extends AlterTable, CanBeSpecifiedAndDerived, DerivationPhase.Phase1 {
      override val tableRef: EntityRef.TableRef = table.tableName
    }

    final case class RenameTable(tableRef: TableRef, newName: String) extends AlterTable, CanOnlyBeSpecified

    final case class DropTable(tableRef: TableRef) extends AlterTable, CanBeSpecifiedAndDerived, DerivationPhase.Phase1

  }

  sealed trait AlterColumn extends StateDiff { self: CoreType =>
    val columnRef: ColumnRef
  }
  object AlterColumn {

    final case class CreateColumn(tableRef: TableRef, column: Column) extends AlterColumn, CanBeSpecifiedAndDerived, DerivationPhase.Phase1 {
      override val columnRef: ColumnRef = ColumnRef(tableRef, column.name)
    }

    final case class DropColumn(columnRef: ColumnRef) extends AlterColumn, CanBeSpecifiedAndDerived, DerivationPhase.Phase1

    final case class RenameColumn(columnRef: ColumnRef, newName: String) extends AlterColumn, CanOnlyBeSpecified

    final case class SetNullable(columnRef: ColumnRef, nullable: Boolean) extends AlterColumn, CanBeSpecifiedAndDerived, DerivationPhase.Phase1

  }

  sealed trait AlterForeignKey extends StateDiff { self: CoreType =>
    val fkRef: ForeignKeyRef
  }
  object AlterForeignKey {

    final case class CreateForeignKey(fk: ForeignKeyState) extends AlterForeignKey, CanBeSpecifiedAndDerived, DerivationPhase.Phase2 {
      override val fkRef: ForeignKeyRef = fk.ref
    }

    final case class RenameExplicitlyNamedForeignKey(fkRef: ForeignKeyRef, newName: String) extends AlterForeignKey, CanOnlyBeSpecified

    final case class RenameAutoNamedForeignKey(fkRef: ForeignKeyRef, newName: String) extends AlterForeignKey, CanOnlyBeDerived, DerivationPhase.Phase3

    final case class DropForeignKey(fkRef: ForeignKeyRef) extends AlterForeignKey, CanBeSpecifiedAndDerived, DerivationPhase.Phase2

  }

}
