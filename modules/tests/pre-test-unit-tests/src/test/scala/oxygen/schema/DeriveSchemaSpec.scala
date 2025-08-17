package oxygen.schema

import oxygen.json.*
import oxygen.predef.test.*
import oxygen.schema.compiled.*

object DeriveSchemaSpec extends OxygenSpecDefault {

  final case class Product1(
      field1: String,
      field2: Option[Boolean],
  ) derives JsonSchema.ProductLike

  final case class Product2(
      @jsonField("product") p: Product1,
  ) derives JsonSchema.ProductLike

  final case class Product3(
      value: String,
      rec: Option[Product3],
  ) derives JsonSchema.ProductLike

  enum MyEnum { case A, B, C }
  object MyEnum extends Enum.Companion[MyEnum]

  enum Sum1 derives JsonSchema.ProductLike {
    @jsonType("_A_") case A
    case B()
    case C(@jsonField("p2") p: Product2)
  }

  @jsonDiscriminator("type")
  enum Sum2 derives JsonSchema.ProductLike {
    case A
    @jsonType("_B_") case B()
    case C(@jsonField("p2") p: Product2, p3: Product3, e: MyEnum)
  }

  override def testSpec: TestSpec =
    suite("DeriveSchemaSpec")(
      suite("Product1")(
        test("works") {
          val schema: JsonSchema.ProductSchema[Product1] = JsonSchema[Product1].asInstanceOf[JsonSchema.ProductSchema[Product1]]
          val fields = schema.fields.toList
          assertTrue(
            fields.map(_.name) == List("field1", "field2"),
            fields.find(_.name == "field1").exists(_.schema == JsonSchema.string),
          )
        },
      ),
      suite("Product2")(
        test("works") {
          val schema: JsonSchema.ProductSchema[Product2] = JsonSchema[Product2].asInstanceOf[JsonSchema.ProductSchema[Product2]]
          val fields = schema.fields.toList
          assertTrue(
            fields.map(_.name) == List("product"),
            fields.find(_.name == "product").exists(_.schema == JsonSchema[Product1]),
          )
        },
      ),
      suite("Sum1")(
        test("works") {
          val schema: JsonSchema.SumSchema[Sum1] = JsonSchema[Sum1].asInstanceOf[JsonSchema.SumSchema[Sum1]]
          val cases = schema.children.toList
          val a = cases.find(_.name == "_A_").get.schema.asInstanceOf[JsonSchema.ProductSchema[Sum1.A.type]]
          val b = cases.find(_.name == "B").get.schema.asInstanceOf[JsonSchema.ProductSchema[Sum1.B]]
          val c = cases.find(_.name == "C").get.schema.asInstanceOf[JsonSchema.ProductSchema[Sum1.C]]
          val aFields = a.fields.toList
          val bFields = b.fields.toList
          val cFields = c.fields.toList
          assertTrue(
            schema.discriminator.isEmpty,
            aFields.map(_.name) == List(),
            bFields.map(_.name) == List(),
            cFields.map(_.name) == List("p2"),
            cFields.find(_.name == "p2").exists(_.schema == JsonSchema[Product2]),
          )
        },
      ),
      suite("Sum2")(
        test("works") {
          val schema: JsonSchema.SumSchema[Sum2] = JsonSchema[Sum2].asInstanceOf[JsonSchema.SumSchema[Sum2]]
          val cases = schema.children.toList
          val a = cases.find(_.name == "A").get.schema.asInstanceOf[JsonSchema.ProductSchema[Sum2.A.type]]
          val b = cases.find(_.name == "_B_").get.schema.asInstanceOf[JsonSchema.ProductSchema[Sum2.B]]
          val c = cases.find(_.name == "C").get.schema.asInstanceOf[JsonSchema.ProductSchema[Sum2.C]]
          val aFields = a.fields.toList
          val bFields = b.fields.toList
          val cFields = c.fields.toList

          assertTrue(
            schema.discriminator.contains("type"),
            aFields.map(_.name) == List(),
            bFields.map(_.name) == List(),
            cFields.map(_.name) == List("p2"),
            cFields.find(_.name == "p2").exists(_.schema == JsonSchema[Product2]),
          )
        },
      ),
      test("compiler") {
        val c1 = Compiler.initial
        val (r1, c2) = c1.compile(JsonSchema[Product1])
        val (r2, c3) = c2.compile(JsonSchema[Product3])
        val (r3, c4) = c3.compile(JsonSchema[Sum1])
        val (r4, c5) = c4.compile(JsonSchema[Sum2])
        val (r5, c6) = c5.compile(JsonSchema[Option[Product2]])
        val (r6, c7) = c6.compile(JsonSchema[List[Product2]])

        println()
        println(Seq(r1, r2, r3, r4, r5, r6).mkString("\n"))
        println()
        println(c7)
        println()

        // TODO (KR) : Array[Specified[A]] == Array[A]
        //           : specified: missing
        //           : option:    missing/nullable
        //           : Specified[A] anywhere besides a json object field is the same as [A]

        assertTrue(
          r1 == TypeRef.ConcreteJsonRef(TypeTag[Product1]),
          r2 == TypeRef.ConcreteJsonRef(TypeTag[Product3]),
          r3 == TypeRef.ConcreteJsonRef(TypeTag[Sum1]),
          r4 == TypeRef.ConcreteJsonRef(TypeTag[Sum2]),
          r5 == TypeRef.JsonOption(TypeRef.ConcreteJsonRef(TypeTag[Product2])),
          r6 == TypeRef.JsonArray(TypeRef.ConcreteJsonRef(TypeTag[Product2])),
        )
      },
    )

}
