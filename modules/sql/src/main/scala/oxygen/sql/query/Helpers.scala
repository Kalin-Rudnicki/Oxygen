package oxygen.sql.query

import oxygen.sql.schema.TableRepr

object Helpers {

  def selectByKey[R](using tableRepr: TableRepr[R, ?]): QueryIO[tableRepr.KeyT, R] = {
    val letter = tableRepr.tableName.head.toString
    QueryIO.simple(
      s"Select ${tableRepr.ref}",
      QueryContext.QueryType.Select,
    )(tableRepr.pk.rowRepr.encoder, tableRepr.rowRepr.decoder)(
      s"""SELECT ${tableRepr.rowRepr.columns.`ref.a, ref.b, ref.c`(letter)}
         |  FROM ${tableRepr.ref} $letter
         |  WHERE ${tableRepr.pk.rowRepr.columns.`(ref.a, ref.b, ref.c)`(letter)} = ${tableRepr.pk.rowRepr.columns.`(?, ?, ?)`}""".stripMargin,
    )
  }

  def insertInto[R](using tableRepr: TableRepr[R, ?]): QueryI[R] =
    QueryI.simple(
      s"Insert ${tableRepr.ref}",
      QueryContext.QueryType.Insert,
    )(tableRepr.rowRepr.encoder)(
      s"""INSERT
         |  INTO ${tableRepr.ref} (${tableRepr.rowRepr.columns.`a, b, c`})
         |  VALUES (${tableRepr.rowRepr.columns.`?, ?, ?`})""".stripMargin,
    )

  def updateByKey[R](using tableRepr: TableRepr[R, ?]): QueryI[R] = {
    val letter = tableRepr.tableName.head.toString
    QueryI.simple(
      s"Update ${tableRepr.ref}",
      QueryContext.QueryType.Update,
    )(
      tableRepr.nonPK.aEncoder ~ tableRepr.pk.aEncoder,
    )(
      s"""UPDATE ${tableRepr.schemaName}.${tableRepr.tableName} $letter
         |  SET${tableRepr.nonPK.rowRepr.columns.columns.map { c => s"\n    ${c.name} = ?" }.mkString(",")}
         |  WHERE ${tableRepr.pk.rowRepr.columns.`(ref.a, ref.b, ref.c)`(letter)} = ${tableRepr.pk.rowRepr.columns.`(?, ?, ?)`}""".stripMargin,
    )
  }

}
