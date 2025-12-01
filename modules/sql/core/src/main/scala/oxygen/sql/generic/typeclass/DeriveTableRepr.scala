package oxygen.sql.generic.typeclass

import oxygen.meta.{*, given}
import oxygen.predef.core.*
import oxygen.quoted.*
import oxygen.sql.*
import oxygen.sql.schema.*
import scala.quoted.*

final class DeriveTableRepr[A: Type](
    instances: K0.Expressions[RowRepr, A],
)(using quotes: Quotes, fTpe: Type[RowRepr], generic: K0.ProductGeneric[A]) {

  private def schemaNameExpr: Expr[String] =
    Expr { generic.annotations.optionalOfValue[schemaName].fold("public")(_.name) }

  private def tableNameExpr: Expr[String] =
    Expr { generic.annotations.optionalOfValue[tableName].fold(generic.label.camelToSnake)(_.name) }

  private def rowReprExpr: Expr[RowRepr.ProductRepr[A]] =
    DeriveProductRowRepr[A](instances).derive

  private def makePartial(isPK: Boolean): (TypeRepr, Expr[TableRepr.Partial[A, ?]]) = {
    type B
    val subset: K0.ProductGeneric.Subset[A, B] =
      generic.filtered[B] { [i] => (_, _) ?=> (field: generic.Field[i]) =>
        field.annotations.optionalOfValue[primaryKey].nonEmpty == isPK
      }
    given Type[B] = subset.bTpe

    val rowReprExpr: Expr[RowRepr[B]] =
      subset.subInstance.fromDeriver[RowRepr](
        DeriveProductRowRepr(_),
        instances,
        '{ RowRepr.Empty },
      )

    subset.bGeneric.typeRepr ->
      '{
        TableRepr.Partial[A, B](
          _get = ${ subset.convertExpr },
          rowRepr = $rowReprExpr,
        )
      }
  }

  private object foreignKeys {

    private final case class ForeignKeyField[PGT, FT](gen: K0.ProductGeneric[PGT], field: gen.Field[FT]) {

      private given Type[PGT] = gen.tpe
      private given Type[FT] = field.tpe

      def toColumns(in: Expr[TableRepr[PGT]])(using Quotes): Expr[ArraySeq[Column]] =
        '{ $in.rowRepr.unsafeChild[FT](${ Expr(field.name) }).columns.columns }

    }
    private object ForeignKeyField {

      def parse[PGT](gen: K0.ProductGeneric[PGT], fExpr: Expr[PGT => Any])(using quotes: Quotes): ForeignKeyField[PGT, ?] = {
        val defDef: DefDef =
          fExpr.toTerm.removeInlineAndTyped match {
            case Block(List(defDef: DefDef), _: Closure) => defDef
            case _                                       => report.errorAndAbort("expected single closure and defDef")
          }
        val defDefRhs: Term = defDef.requiredRHS

        val fieldName: String =
          defDefRhs match {
            case Select(ident: Ident, fieldName) if ident.tpe.widen =:= gen.typeRepr => fieldName
            case _                                                                   => report.errorAndAbort("Invalid field select", defDefRhs.pos)
          }

        val field: gen.Field[?] =
          gen.fields.find(_.name == fieldName).getOrElse { report.errorAndAbort(s"No such field in ${gen.typeRepr.showAnsiCode} named $fieldName", defDefRhs.pos) }

        ForeignKeyField(gen, field)
      }

    }

    private final case class ForeignKeyPair[RT, FT] private (current: ForeignKeyField[A, FT], references: ForeignKeyField[RT, FT])
    private object ForeignKeyPair {

      def validate[RT](current: ForeignKeyField[A, ?], references: ForeignKeyField[RT, ?], pos: Position)(using q: Quotes): ForeignKeyPair[RT, ?] = {
        type FT

        val opt = TypeRepr.of[Option[?]].narrow[AppliedType].tycon
        extension (self: TypeRepr)
          def removeOption: TypeRepr =
            self.widen.dealias match
              case AppliedType(repr, arg :: Nil) if repr =:= opt => arg
              case t                                             => t

        if !(current.field.typeRepr.removeOption =:= references.field.typeRepr.removeOption) then report.errorAndAbort("foreign key field types do not align", pos)

        ForeignKeyPair(current.asInstanceOf[ForeignKeyField[A, FT]], references.asInstanceOf[ForeignKeyField[RT, FT]])
      }

      def parse[RT](referencesGen: K0.ProductGeneric[RT], currentFExpr: Expr[A => Any], referencesFExpr: Expr[RT => Any], pos: Position)(using Quotes): ForeignKeyPair[RT, ?] =
        validate(ForeignKeyField.parse(generic, currentFExpr), ForeignKeyField.parse(referencesGen, referencesFExpr), pos)

    }

    private def classForeignKey(expr: Expr[foreignKey[A, ?]])(using quotes: Quotes): Expr[ForeignKeyRepr[A, ?]] = {
      type RT
      given Type[RT] = expr match
        case '{ `foreignKey`[A, rt](${ _ }*) }     => TypeRepr.of[rt].asTypeOf
        case '{ new `foreignKey`[A, rt](${ _ }*) } => TypeRepr.of[rt].asTypeOf
        case _                                     => report.errorAndAbort("unable to extract reference type", expr)

      val typedExpr: Expr[foreignKey[A, RT]] = expr.asExprOf[foreignKey[A, RT]]

      val pairsList: Seq[Expr[(A => Any, RT => Any)]] = typedExpr match
        case '{ `foreignKey`[A, RT](${ Varargs(exprs) }*) }     => exprs
        case '{ new `foreignKey`[A, RT](${ Varargs(exprs) }*) } => exprs
        case _                                                  => report.errorAndAbort("unable to extract foreign key expr pairs", expr)

      type RT_FK
      val rtGeneric: K0.ProductGeneric[RT] = K0.ProductGeneric.of[RT]

      val rtFkSubset: K0.ProductGeneric.Subset[RT, RT_FK] =
        rtGeneric.filtered[RT_FK] { [i] => (_, _) ?=> (field: rtGeneric.Field[i]) =>
          field.annotations.optionalOfValue[primaryKey].nonEmpty
        }

      if rtFkSubset.bGeneric.children.isEmpty then report.errorAndAbort(s"${rtGeneric.typeRepr.showAnsiCode} does not have a primary key", expr)

      if pairsList.size != rtFkSubset.bGeneric.children.size then
        report.errorAndAbort(
          s"foreign key pairs do not have same size as the primary key of ${rtGeneric.typeRepr.showAnsiCode} (${pairsList.size} != ${rtFkSubset.bGeneric.children.size})",
          expr,
        )

      val pairs: Seq[ForeignKeyPair[RT, ?]] =
        pairsList.map {
          case e @ '{ (${ currentFExpr }: A => Any, ${ referencesFExpr }: RT => Any) } => ForeignKeyPair.parse(rtGeneric, currentFExpr, referencesFExpr, e.toTerm.pos)
          case expr                                                                    => report.errorAndAbort("invalid format, expected: (_.currentField, _.referencesField)", expr)
        }

      val rtTableReprExpr: Expr[TableRepr[RT]] = rtGeneric.summonTypeClass[TableRepr]

      '{
        ForeignKeyRepr[A, RT](
          None, // TODO (KR) : allow for explicit fk naming
          $rtTableReprExpr,
          currentTableRepr => ArraySeq(${ pairs.map(_.current.toColumns('currentTableRepr)).seqToArraySeqExpr }*).flatten,
          referencesTableRepr => ArraySeq(${ pairs.map(_.references.toColumns('referencesTableRepr)).seqToArraySeqExpr }*).flatten,
        )
      }
    }

    private def classForeignKeys(using Quotes): Growable[Expr[ForeignKeyRepr[A, ?]]] = {
      val allAnnotsUntyped: List[Expr[foreignKey[?, ?]]] = generic.annotations.allOf[foreignKey[?, ?]]
      val allAnnotsTyped: List[Expr[foreignKey[A, ?]]] =
        allAnnotsUntyped.map {
          case annot if annot.isExprOf[foreignKey[A, ?]] => annot.asExprOf[foreignKey[A, ?]]
          case annot                                     => report.errorAndAbort(s"foreignKey.Current does not match current class ${generic.typeRepr.showAnsiCode}", annot)
        }
      Growable.many(allAnnotsTyped).map(classForeignKey(_))
    }

    private def fieldForeignKey[FT](field: generic.Field[FT], expr: Expr[references[?]])(using Quotes): Expr[ForeignKeyRepr[A, ?]] = {
      type RT
      given Type[RT] = expr match
        case '{ `references`[rt]() }     => TypeRepr.of[rt].asTypeOf
        case '{ new `references`[rt]() } => TypeRepr.of[rt].asTypeOf
        case '{ new `references`[rt] }   => TypeRepr.of[rt].asTypeOf
        case _                           => report.errorAndAbort("unable to extract reference type", expr)

      type RT_FK
      val rtGeneric: K0.ProductGeneric[RT] = K0.ProductGeneric.of[RT]

      val rtFkSubset: K0.ProductGeneric.Subset[RT, RT_FK] =
        rtGeneric.filtered[RT_FK] { [i] => (_, _) ?=> (field: rtGeneric.Field[i]) =>
          field.annotations.optionalOfValue[primaryKey].nonEmpty
        }

      if rtFkSubset.bGeneric.children.size != 1 then report.errorAndAbort(s"${rtGeneric.typeRepr.showAnsiCode} does not have a single primary key", expr)

      val ftFkField: rtGeneric.Field[RT_FK] =
        rtFkSubset.toSingle.aField.asInstanceOf[rtGeneric.Field[RT_FK]]

      val pair: ForeignKeyPair[RT, ?] = ForeignKeyPair.validate(ForeignKeyField(generic, field), ForeignKeyField(rtGeneric, ftFkField), expr.toTerm.pos)

      val rtTableReprExpr: Expr[TableRepr[RT]] = rtGeneric.summonTypeClass[TableRepr]

      '{
        ForeignKeyRepr[A, RT](
          None, // TODO (KR) : allow for explicit fk naming
          $rtTableReprExpr,
          currentTableRepr => ${ pair.current.toColumns('currentTableRepr) },
          referencesTableRepr => ${ pair.references.toColumns('referencesTableRepr) },
        )
      }
    }

    private def fieldForeignKeys(_field: generic.Field[?])(using Quotes): Growable[Expr[ForeignKeyRepr[A, ?]]] = {
      type T
      val field: generic.Field[T] = _field.typedAs[T]
      val allAnnotsUntyped: List[Expr[references[?]]] = field.annotations.allOf[references[?]]
      Growable.many(allAnnotsUntyped).map(fieldForeignKey(field, _))
    }

    def all(using Quotes): Expr[ArraySeq[ForeignKeyRepr[A, ?]]] =
      (classForeignKeys ++ Growable.many(generic.fields).flatMap(fieldForeignKeys)).seqToArraySeqExpr

  }

  private object indices {

    private final case class IndexField[PGT, FT](gen: K0.ProductGeneric[PGT], field: gen.Field[FT]) {

      private given Type[PGT] = gen.tpe
      private given Type[FT] = field.tpe

      def toColumns(in: Expr[TableRepr[PGT]])(using Quotes): Expr[ArraySeq[Column]] =
        '{ $in.rowRepr.unsafeChild[FT](${ Expr(field.name) }).columns.columns }

    }
    private object IndexField {

      def parse[PGT](gen: K0.ProductGeneric[PGT], fExpr: Expr[PGT => Any])(using quotes: Quotes): IndexField[PGT, ?] = {
        val defDef: DefDef =
          fExpr.toTerm.removeInlineAndTyped match {
            case Block(List(defDef: DefDef), _: Closure) => defDef
            case _                                       => report.errorAndAbort("expected single closure and defDef")
          }
        val defDefRhs: Term = defDef.requiredRHS

        val fieldName: String =
          defDefRhs match {
            case Select(ident: Ident, fieldName) if ident.tpe.widen =:= gen.typeRepr => fieldName
            case _                                                                   => report.errorAndAbort("Invalid field select", defDefRhs.pos)
          }

        val field: gen.Field[?] =
          gen.fields.find(_.name == fieldName).getOrElse { report.errorAndAbort(s"No such field in ${gen.typeRepr.showAnsiCode} named $fieldName", defDefRhs.pos) }

        IndexField(gen, field)
      }

    }

    private def classIndex(expr: Expr[index[A]])(using quotes: Quotes): Expr[IndexRepr[A]] = {
      val isUnique: Boolean = expr match
        case '{ `index`[A](${ _ }*) }              => false
        case '{ new `index`[A](${ _ }*) }          => false
        case '{ `index`.`unique`[A](${ _ }*) }     => true
        case '{ new `index`.`unique`[A](${ _ }*) } => true
        case _                                     => report.errorAndAbort("unable to extract index uniqueness", expr)

      val fieldList: Seq[Expr[A => Any]] = expr match
        case '{ `index`[A](${ Varargs(exprs) }*) }              => exprs
        case '{ new `index`[A](${ Varargs(exprs) }*) }          => exprs
        case '{ `index`.`unique`[A](${ Varargs(exprs) }*) }     => exprs
        case '{ new `index`.`unique`[A](${ Varargs(exprs) }*) } => exprs
        case _                                                  => report.errorAndAbort("unable to extract index fields", expr)

      val fields: Seq[IndexField[A, ?]] =
        fieldList.map { ff => IndexField.parse(generic, ff) }

      '{
        IndexRepr[A](
          None, // TODO (KR) : allow for explicit fk naming
          ${ Expr(isUnique) },
          currentTableRepr => ArraySeq(${ fields.map(_.toColumns('currentTableRepr)).seqToArraySeqExpr }*).flatten,
        )
      }
    }

    private def classIndices(using Quotes): Growable[Expr[IndexRepr[A]]] = {
      val allAnnotsUntyped: List[Expr[index[?]]] = generic.annotations.allOf[index[?]]
      val allAnnotsTyped: List[Expr[index[A]]] =
        allAnnotsUntyped.map {
          case annot if annot.isExprOf[index[A]] => annot.asExprOf[index[A]]
          case annot                             => report.errorAndAbort(s"index.Current does not match current class ${generic.typeRepr.showAnsiCode}", annot)
        }
      Growable.many(allAnnotsTyped).map(classIndex(_))
    }

    private def fieldIndex[FT](field: generic.Field[FT], expr: Expr[indexed])(using Quotes): Expr[IndexRepr[A]] = {
      val isUnique: Boolean = expr match
        case '{ `indexed`() }              => false
        case '{ new `indexed`() }          => false
        case '{ `indexed`.`unique`() }     => true
        case '{ new `indexed`.`unique`() } => true
        case _                             => report.errorAndAbort("unable to extract index uniqueness", expr)

      val idxField: IndexField[A, ?] = IndexField(generic, field)

      '{
        IndexRepr[A](
          None, // TODO (KR) : allow for explicit idx naming
          ${ Expr(isUnique) },
          currentTableRepr => ${ idxField.toColumns('currentTableRepr) },
        )
      }
    }

    private def fieldIndices(_field: generic.Field[?])(using Quotes): Growable[Expr[IndexRepr[A]]] = {
      type T
      val field: generic.Field[T] = _field.typedAs[T]
      val allAnnotsUntyped: List[Expr[indexed]] = field.annotations.allOf[indexed]
      Growable.many(allAnnotsUntyped).map(fieldIndex(field, _))
    }

    def all(using Quotes): Expr[ArraySeq[IndexRepr[A]]] =
      (classIndices ++ Growable.many(generic.fields).flatMap(fieldIndices)).seqToArraySeqExpr

  }

  def derive: Expr[TableRepr.Typed[A, ?, ?]] = {
    val (pkTpe, pkPartial) = makePartial(true)
    val (npkTpe, npkPartial) = makePartial(false)

    type PK
    type NPK

    given Type[PK] = pkTpe.asTypeOf
    given Type[NPK] = npkTpe.asTypeOf

    '{
      TableRepr.TypedImpl[A, PK, NPK](
        schemaName = $schemaNameExpr,
        tableName = $tableNameExpr,
        rowRepr = $rowReprExpr,
        pk = ${ pkPartial.asExprOf[TableRepr.Partial[A, PK]] },
        npk = ${ npkPartial.asExprOf[TableRepr.Partial[A, NPK]] },
        foreignKeys = ${ foreignKeys.all },
        indices = ${ indices.all },
      )
    }
  }

}
