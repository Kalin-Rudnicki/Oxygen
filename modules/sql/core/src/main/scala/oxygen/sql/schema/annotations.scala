package oxygen.sql.schema

import oxygen.meta.FromExprT
import oxygen.predef.core.*
import scala.annotation.*
import scala.quoted.*

/**
  * Explicitly set the name of the schema a table is in.
  */
final case class schemaName(name: String) extends StaticAnnotation derives FromExprT

/**
  * Explicitly set the name of a table.
  */
final case class tableName(name: String) extends StaticAnnotation derives FromExprT

/**
  * Explicitly set the name of a column.
  */
final case class columnName(name: String) extends StaticAnnotation derives FromExprT

/**
  * Best explained with an example:
  * `final case class Inner(field_1: String, field_2: String) derives RowSchema.ProductSchema`
  * `final case class Outer(inner: Inner) derives RowSchema.ProductSchema`
  *
  * By default, this schema will have 2 fields: `inner_field_1` and `inner_field_2`.
  * Using [[inlineColumnNames]] on `inner`, like:
  * `final case class Outer(@inlineColumnName inner: Inner) derives RowSchema.ProductSchema`
  * will result in fields `field_1` and `field_2`.
  *
  * NOTE: If this is used on field that is not a product schema, the annotation will be ignored.
  */
final case class inlineColumnNames() extends StaticAnnotation derives FromExprT

/**
  * Denotes that this field should be a primary key for the table.
  */
final case class primaryKey() extends StaticAnnotation derives FromExprT

/**
  * Marks a field as a foreign key referencing `References`; the constraint is auto-named.
  *
  * To pin an explicit constraint name, use [[references.named]].
  */
class references[References]() extends StaticAnnotation
object references {

  /** A field-level foreign key with an explicit constraint `name`. */
  final class named[References](val name: String) extends references[References]

}

/**
  * Defines a class-level foreign key from `Current` to `References`; the constraint is auto-named.
  *
  * To pin an explicit constraint name, use [[foreignKey.named]].
  */
class foreignKey[Current, References](refs: (Current => Any, References => Any)*) extends StaticAnnotation
object foreignKey {

  /** A class-level foreign key with an explicit constraint `name`. */
  final class named[Current, References](val name: String, refs: (Current => Any, References => Any)*) extends foreignKey[Current, References](refs*)

}

/**
  * Marks a field as indexed; the index is auto-named.
  *
  * `indexed.unique` for a unique index; `indexed.named` / `indexed.unique.named` to pin an explicit index name.
  */
class indexed extends StaticAnnotation
object indexed {
  class unique extends indexed
  object unique {

    /** A field-level unique index with an explicit `name`. */
    final class named(val name: String) extends indexed.unique

  }

  /** A field-level index with an explicit `name`. */
  final class named(val name: String) extends indexed

}

/**
  * Defines a class-level index over the given columns; the index is auto-named.
  *
  * `index.unique` for a unique index; `index.named` / `index.unique.named` to pin an explicit index name.
  */
class index[Current](cols: (Current => Any)*) extends StaticAnnotation
object index {
  class unique[Current](cols: (Current => Any)*) extends index[Current](cols*)
  object unique {

    /** A class-level unique index with an explicit `name`. */
    final class named[Current](val name: String, cols: (Current => Any)*) extends index.unique[Current](cols*)

  }

  /** A class-level index with an explicit `name`. */
  final class named[Current](val name: String, cols: (Current => Any)*) extends index[Current](cols*)

}
