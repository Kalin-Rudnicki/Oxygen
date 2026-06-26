package oxygen.schema

import oxygen.json.*
import oxygen.predef.test.*
import oxygen.schema.compiled.*

object JsonSchemaEmitterSpec extends OxygenSpecDefault {

  //////////////////////////////////////////////////////////////////////////////////////////////////////
  //      Test types
  //////////////////////////////////////////////////////////////////////////////////////////////////////

  enum Color derives StrictEnum { case Red, Green, Blue }

  final case class Person(
      name: String,
      age: Int,
      nickname: Option[String],
      favoriteColor: Color,
  ) derives JsonSchema

  final case class Team(
      lead: Person,
      members: List[Person],
  ) derives JsonSchema

  final case class Tree(
      value: Int,
      children: List[Tree],
  ) derives JsonSchema

  //////////////////////////////////////////////////////////////////////////////////////////////////////
  //      Helpers
  //////////////////////////////////////////////////////////////////////////////////////////////////////

  private def standalone[A: JsonSchema]: Json = {
    val out = Compiled.usingJson[A].compiled
    JsonSchemaEmitter.emitStandalone(FullCompiledSchemas(out.schemas), out.value)
  }

  extension (j: Json) {
    private def fld(name: String): Json =
      j match
        case Json.Obj(fs) => fs.collectFirst { case (k, v) if k == name => v }.getOrElse(Json.Null)
        case _            => Json.Null
    private def asString: Option[String] =
      j match
        case Json.Str(s) => s.some
        case _           => None
    private def arrElems: ArraySeq[Json] =
      j match
        case Json.Arr(a) => a
        case _           => ArraySeq.empty
    private def objKeys: Set[String] =
      j match
        case Json.Obj(fs) => fs.iterator.map(_._1).toSet
        case _            => Set.empty
    private def objValues: ArraySeq[Json] =
      j match
        case Json.Obj(fs) => fs.map(_._2)
        case _            => ArraySeq.empty
  }

  /** The (single) `$defs` entry for the root type's body. */
  private def soleDef(root: Json): Json = root.fld("$defs").objValues.head
  private def stringSet(j: Json): Set[String] = j.arrElems.flatMap(_.asString).toSet

  //////////////////////////////////////////////////////////////////////////////////////////////////////
  //      Spec
  //////////////////////////////////////////////////////////////////////////////////////////////////////

  override def testSpec: TestSpec =
    suite("JsonSchemaEmitterSpec")(
      test("dialect + product body shape") {
        val root = standalone[Person]
        val defsKeys = root.fld("$defs").objKeys
        val person = soleDef(root)
        val props = person.fld("properties")
        assertTrue(
          root.fld("$schema").asString.contains(JsonSchemaEmitter.dialect),
          // top-level is a $ref into $defs
          root.fld("$ref").asString.exists(_.startsWith("#/$defs/")),
          defsKeys.size == 1,
          // product => object with all 4 fields, additionalProperties:false
          person.fld("type").asString.contains("object"),
          props.objKeys == Set("name", "age", "nickname", "favoriteColor"),
          person.fld("additionalProperties") == Json.Bool(false),
          // scalar field types
          props.fld("name").fld("type").asString.contains("string"),
          props.fld("age").fld("type").asString.contains("integer"),
        )
      },
      test("required excludes Option fields; nullable Option is anyOf null") {
        val person = soleDef(standalone[Person])
        val required = stringSet(person.fld("required"))
        val nickname = person.fld("properties").fld("nickname")
        // nickname is Option => absent from required, and its schema permits null
        assertTrue(
          required.contains("name"),
          required.contains("age"),
          required.contains("favoriteColor"),
          !required.contains("nickname"),
          nickname.fld("anyOf").arrElems.exists(_.fld("type").asString.contains("null")),
        )
      },
      test("enum field emits inline string + enum (not a $def)") {
        val root = standalone[Person]
        val color = soleDef(root).fld("properties").fld("favoriteColor")
        assertTrue(
          // only the Person product is a $def — the enum is inlined
          root.fld("$defs").objKeys.size == 1,
          color.fld("type").asString.contains("string"),
          stringSet(color.fld("enum")) == Set("Red", "Green", "Blue"),
        )
      },
      test("nested product is a shared $def, referenced by array items + field") {
        val root = standalone[Team]
        val defsKeys = root.fld("$defs").objKeys
        // Team + Person => exactly 2 defs (Person is shared, not duplicated)
        val teamDef = root.fld("$defs").objValues.find(_.fld("properties").objKeys == Set("lead", "members")).get
        val members = teamDef.fld("properties").fld("members")
        val lead = teamDef.fld("properties").fld("lead")
        assertTrue(
          defsKeys.size == 2,
          members.fld("type").asString.contains("array"),
          members.fld("items").fld("$ref").asString.exists(_.startsWith("#/$defs/")),
          lead.fld("$ref").asString == members.fld("items").fld("$ref").asString,
        )
      },
      test("recursive type terminates via $ref to itself") {
        val root = standalone[Tree]
        val treeDef = soleDef(root)
        val childrenItemsRef = treeDef.fld("properties").fld("children").fld("items").fld("$ref").asString
        val treeDefName = root.fld("$defs").objKeys.head
        assertTrue(
          root.fld("$defs").objKeys.size == 1,
          childrenItemsRef.contains(s"#/$$defs/$treeDefName"),
        )
      },
    )

}
