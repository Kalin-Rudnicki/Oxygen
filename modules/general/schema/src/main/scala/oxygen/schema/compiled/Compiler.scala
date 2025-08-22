package oxygen.schema.compiled

import oxygen.schema.{compiled as C, intermediate as I, *}

final class Compiler(
    intermediate: I.Reprs,
    // compiled: Schemas, // TODO (KR) :
) {

  def compile(schema: AnySchema): (TypeRef, Compiler) = {
    val i2 = I.Compiled.compile(schema, intermediate)
    val r2 = intermediate ++ i2.reprs
    val c2 = Compiler(r2)
    val tr = i2.ref match // FIX-PRE-MERGE (KR) :
      case r: I.SimpleTypeRef.Plain => C.SchemaRepr.plainTypeRef(r, r2)
      case r: I.SimpleTypeRef.Json  => C.SchemaRepr.jsonTypeRef(r, r2)
    println(s"${schema.typeTag.prefixAll} -> $tr")
    (tr, c2)
  }

  def complete: Any =
    ??? // FIX-PRE-MERGE (KR) :

  // FIX-PRE-MERGE (KR) : remove
  override def toString: String =
    s"""--- Compiler ---
       |plain:${intermediate.plain.toSeq.sortBy(_._1.typeTag.prefixObject).map { case (k, v) => s"\n  - $k: $v" }.mkString}
       |json:${intermediate.json.toSeq.sortBy(_._1.typeTag.prefixObject).map { case (k, v) => s"\n  - $k: $v" }.mkString}""".stripMargin

}
object Compiler {

  val initial: Compiler = Compiler(I.Reprs.empty)

}
