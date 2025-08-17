package oxygen.schema.compiled

import oxygen.predef.core.*
import oxygen.schema.{compiled as C, intermediate as I, *}

final class SchemaCompiler(private val reprs: I.IntermediateReprs) {

  private[compiled] def compilePlain(schema: PlainTextSchema[?]): (CompiledSchemaRef.PlainLike, SchemaCompiler) = {
    val compiledPlain: I.IntermediateCompiledRef.Plain = I.IntermediateCompiledRef.compilePlain(schema, reprs)
    val newCompiler: SchemaCompiler = SchemaCompiler(reprs ++ compiledPlain.reprs)
    (CompiledSchemaRef.resolvePlain(compiledPlain.ref, newCompiler.reprs), newCompiler)
  }

  private[compiled] def compileJson(schema: JsonSchema[?]): (CompiledSchemaRef.JsonLike, SchemaCompiler) = {
    val compiledJson: I.IntermediateCompiledRef.Json = I.IntermediateCompiledRef.compileJson(schema, reprs)
    val newCompiler: SchemaCompiler = SchemaCompiler(reprs ++ compiledJson.reprs)
    (CompiledSchemaRef.resolveJson(compiledJson.ref, newCompiler.reprs), newCompiler)
  }

  def finalizedSchemas: RawCompiledSchemas = {
    val tmpPlain: Seq[RawCompiledPlainSchema] = reprs.plain.toSeq.flatMap { RawCompiledPlainSchema.from(_, _, reprs).toOption }
    val tmpJson: Seq[RawCompiledJsonSchema] = reprs.json.toSeq.flatMap { RawCompiledJsonSchema.from(_, _, reprs).toOption }

    // it seems the initial duplicate detection logic doesn't quite catch all the cases
    // it might be the case that improving that logic would remove the need for this second pass
    // but for the time being, this catches the other cases
    // in order to explore what is not caught, MapFunction.deDuplicate could be replaced with MapFunction.id
    val mapFunc: TypeIdentifier.MapFunction = TypeIdentifier.MapFunction.deDuplicate((tmpPlain ++ tmpJson).map(_.typeIdentifier).distinct)

    RawCompiledSchemas(
      tmpPlain.map(_.mapTypeIdentifier(mapFunc)).toArraySeq.sortBy(_.typeIdentifier),
      tmpJson.map(_.mapTypeIdentifier(mapFunc)).toArraySeq.sortBy(_.typeIdentifier),
    )
  }

  override def toString: String = {
    val plainPairs: Seq[(String, String)] = reprs.plain.toSeq.sortBy(_._1.typeTag.prefixObject).map { case (k, v) => (k.typeTag.toString, v.toString) }
    val jsonPairs: Seq[(String, String)] = reprs.json.toSeq.sortBy(_._1.typeTag.prefixObject).map { case (k, v) => (k.typeTag.toString, v.toString) }

    val maxLen: Int = (plainPairs ++ jsonPairs).map(_._1.length).maxOption.getOrElse(0)

    s"""--- Compiler ---
       |plain:${plainPairs.map { case (k, v) => s"\n  -   ${k.alignRight(maxLen)}  :  $v" }.mkString}
       |json:${jsonPairs.map { case (k, v) => s"\n  -   ${k.alignRight(maxLen)}  :  $v" }.mkString}""".stripMargin
  }

}
object SchemaCompiler {

  val initial: SchemaCompiler = SchemaCompiler(I.IntermediateReprs.empty)

}
