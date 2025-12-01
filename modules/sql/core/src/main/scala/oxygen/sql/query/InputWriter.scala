package oxygen.sql.query

import oxygen.sql.schema.Column

final class InputWriter(ps: java.sql.PreparedStatement) {

  private var idx: Int = 1

  private[query] def putBatch(): Unit = {
    ps.addBatch()
    ps.clearParameters()
    idx = 1
  }

  def unsafeWrite(any: Any): Unit = {
    ps.setObject(idx, any)
    idx += 1
  }

  def unsafeWriteArray(colType: Column.Type, value: Array[Object]): Unit = {
    ps.setArray(idx, ps.getConnection.createArrayOf(colType.baseType, value))
    idx += 1
  }

  def writeNulls(num: Int): Unit = {
    var c: Int = 0
    while c < num do {
      unsafeWrite(null)
      c += 1
    }
  }

}
