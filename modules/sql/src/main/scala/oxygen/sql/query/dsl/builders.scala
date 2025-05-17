package oxygen.sql.query.dsl

import oxygen.sql.schema.TableRepr

object select {
  def apply(queryName: String): SelectNoInput = macroOnly
  def input[I](queryName: String): SelectInput[I] = macroOnly
}

def from[A](using schema: TableRepr[A, ?]): From[A] = macroOnly
def join[A](using schema: TableRepr[A, ?]): Join[A] = macroOnly

def where: Where = macroOnly
