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
