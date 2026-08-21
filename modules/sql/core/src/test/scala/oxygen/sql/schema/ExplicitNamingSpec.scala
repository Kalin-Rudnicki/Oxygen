package oxygen.sql.schema

import java.util.UUID
import oxygen.predef.test.*
import oxygen.sql.query.TableCompanion

object ExplicitNamingSpec extends OxygenSpecDefault {

  final case class Parent(@primaryKey id: UUID, name: String)
  object Parent extends TableCompanion[Parent, UUID](TableRepr.derived[Parent])

  // --- field-level foreign keys (@references) ---

  final case class ChildFkAuto(@primaryKey id: UUID, @references[Parent] parentId: UUID)
  object ChildFkAuto extends TableCompanion[ChildFkAuto, UUID](TableRepr.derived[ChildFkAuto])

  final case class ChildFkNamed(@primaryKey id: UUID, @references.named[Parent]("fk_child_parent") parentId: UUID)
  object ChildFkNamed extends TableCompanion[ChildFkNamed, UUID](TableRepr.derived[ChildFkNamed])

  // --- class-level foreign keys (@foreignKey) ---

  @foreignKey[ClassFkAuto, Parent]((_.parentId, _.id))
  final case class ClassFkAuto(@primaryKey id: UUID, parentId: UUID)
  object ClassFkAuto extends TableCompanion[ClassFkAuto, UUID](TableRepr.derived[ClassFkAuto])

  @foreignKey.named[ClassFkNamed, Parent]("fk_class_parent", (_.parentId, _.id))
  final case class ClassFkNamed(@primaryKey id: UUID, parentId: UUID)
  object ClassFkNamed extends TableCompanion[ClassFkNamed, UUID](TableRepr.derived[ClassFkNamed])

  // --- field-level indices (@indexed) ---

  final case class IdxFieldAuto(@primaryKey id: UUID, @indexed name: String)
  object IdxFieldAuto extends TableCompanion[IdxFieldAuto, UUID](TableRepr.derived[IdxFieldAuto])

  final case class IdxFieldNamed(@primaryKey id: UUID, @indexed.named("idx_field_name") name: String)
  object IdxFieldNamed extends TableCompanion[IdxFieldNamed, UUID](TableRepr.derived[IdxFieldNamed])

  final case class IdxFieldUniqueNamed(@primaryKey id: UUID, @indexed.unique.named("idx_field_u_name") name: String)
  object IdxFieldUniqueNamed extends TableCompanion[IdxFieldUniqueNamed, UUID](TableRepr.derived[IdxFieldUniqueNamed])

  // --- class-level indices (@index) ---

  @index[ClassIdxAuto](_.a, _.b)
  final case class ClassIdxAuto(@primaryKey id: UUID, a: Int, b: Int)
  object ClassIdxAuto extends TableCompanion[ClassIdxAuto, UUID](TableRepr.derived[ClassIdxAuto])

  @index.named[ClassIdxNamed]("idx_class_name", _.a, _.b)
  final case class ClassIdxNamed(@primaryKey id: UUID, a: Int, b: Int)
  object ClassIdxNamed extends TableCompanion[ClassIdxNamed, UUID](TableRepr.derived[ClassIdxNamed])

  @index.unique.named[ClassIdxUniqueNamed]("idx_class_u_name", _.a)
  final case class ClassIdxUniqueNamed(@primaryKey id: UUID, a: Int)
  object ClassIdxUniqueNamed extends TableCompanion[ClassIdxUniqueNamed, UUID](TableRepr.derived[ClassIdxUniqueNamed])

  private def onlyFk(repr: TableRepr[?]): ForeignKeyRepr[?, ?] = repr.foreignKeys.head
  private def onlyIdx(repr: TableRepr[?]): IndexRepr[?] = repr.indices.head

  override def testSpec: TestSpec =
    suite("ExplicitNamingSpec")(
      suite("foreign keys")(
        test("field-level @references is auto-named") {
          assertTrue(onlyFk(ChildFkAuto.tableRepr).explicitName.isEmpty)
        },
        test("field-level @references.named carries the explicit name") {
          assertTrue(onlyFk(ChildFkNamed.tableRepr).explicitName.contains("fk_child_parent"))
        },
        test("class-level @foreignKey is auto-named") {
          assertTrue(onlyFk(ClassFkAuto.tableRepr).explicitName.isEmpty)
        },
        test("class-level @foreignKey.named carries the explicit name") {
          assertTrue(onlyFk(ClassFkNamed.tableRepr).explicitName.contains("fk_class_parent"))
        },
      ),
      suite("indices")(
        test("field-level @indexed is auto-named and non-unique") {
          val idx = onlyIdx(IdxFieldAuto.tableRepr)
          assertTrue(idx.explicitName.isEmpty, !idx.unique)
        },
        test("field-level @indexed.named carries the explicit name") {
          val idx = onlyIdx(IdxFieldNamed.tableRepr)
          assertTrue(idx.explicitName.contains("idx_field_name"), !idx.unique)
        },
        test("field-level @indexed.unique.named carries the explicit name and is unique") {
          val idx = onlyIdx(IdxFieldUniqueNamed.tableRepr)
          assertTrue(idx.explicitName.contains("idx_field_u_name"), idx.unique)
        },
        test("class-level @index is auto-named") {
          val idx = onlyIdx(ClassIdxAuto.tableRepr)
          assertTrue(idx.explicitName.isEmpty, !idx.unique)
        },
        test("class-level @index.named carries the explicit name") {
          val idx = onlyIdx(ClassIdxNamed.tableRepr)
          assertTrue(idx.explicitName.contains("idx_class_name"), !idx.unique)
        },
        test("class-level @index.unique.named carries the explicit name and is unique") {
          val idx = onlyIdx(ClassIdxUniqueNamed.tableRepr)
          assertTrue(idx.explicitName.contains("idx_class_u_name"), idx.unique)
        },
      ),
    )

}
