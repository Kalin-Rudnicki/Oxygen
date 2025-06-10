package oxygen.sql.schema

import oxygen.predef.core.*
import scala.annotation.*
import scala.quoted.*

/**
  * Explicitly set the name of the schema a table is in.
  */
final case class schemaName(name: String) extends StaticAnnotation
object schemaName {

  given FromExpr[schemaName] =
    new FromExpr[schemaName] {
      override def unapply(x: Expr[schemaName])(using Quotes): Option[schemaName] = x match
        case '{ new `schemaName`(${ Expr(name) }) }   => schemaName(name).some
        case '{ `schemaName`(${ Expr(name) }) }       => schemaName(name).some
        case '{ `schemaName`.apply(${ Expr(name) }) } => schemaName(name).some
        case _                                        => None
    }

}

/**
  * Explicitly set the name of a table.
  */
final case class tableName(name: String) extends StaticAnnotation
object tableName {

  given FromExpr[tableName] =
    new FromExpr[tableName] {
      override def unapply(x: Expr[tableName])(using Quotes): Option[tableName] = x match
        case '{ new `tableName`(${ Expr(name) }) }   => tableName(name).some
        case '{ `tableName`(${ Expr(name) }) }       => tableName(name).some
        case '{ `tableName`.apply(${ Expr(name) }) } => tableName(name).some
        case _                                       => None
    }

}

/**
  * Explicitly set the name of a column.
  */
final case class columnName(name: String) extends StaticAnnotation
object columnName {

  given FromExpr[columnName] =
    new FromExpr[columnName] {
      override def unapply(x: Expr[columnName])(using Quotes): Option[columnName] = x match
        case '{ new `columnName`(${ Expr(name) }) }   => columnName(name).some
        case '{ `columnName`(${ Expr(name) }) }       => columnName(name).some
        case '{ `columnName`.apply(${ Expr(name) }) } => columnName(name).some
        case _                                        => None
    }

}

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
final class inlineColumnNames extends StaticAnnotation
object inlineColumnNames {

  given FromExpr[inlineColumnNames] =
    new FromExpr[inlineColumnNames] {
      override def unapply(x: Expr[inlineColumnNames])(using Quotes): Option[inlineColumnNames] = x match
        case '{ new `inlineColumnNames`() }   => inlineColumnNames().some
        case '{ new `inlineColumnNames` }     => inlineColumnNames().some
        case '{ `inlineColumnNames`.apply() } => inlineColumnNames().some
        case '{ `inlineColumnNames`() }       => inlineColumnNames().some
        case _                                => None
    }

}

/**
  * Denotes that this field should be a primary key for the table.
  */
final class primaryKey extends StaticAnnotation
object primaryKey {

  given FromExpr[primaryKey] =
    new FromExpr[primaryKey] {
      override def unapply(x: Expr[primaryKey])(using Quotes): Option[primaryKey] = x match
        case '{ new `primaryKey`() }   => primaryKey().some
        case '{ new `primaryKey` }     => primaryKey().some
        case '{ `primaryKey`.apply() } => primaryKey().some
        case '{ `primaryKey`() }       => primaryKey().some
        case _                         => None
    }

}
