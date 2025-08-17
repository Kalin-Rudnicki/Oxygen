package oxygen.schema

import oxygen.json.*
import oxygen.predef.test.*

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

          val reprs = TemporaryRepr.from(schema, TemporaryRepr.Generating(TemporaryRepr.Reprs.empty, Set.empty)).reprs

          val str = IndentedString.inline(SchemaRepr.from(reprs).sortBy(_.typeTag.prefixObject).map(_.toIndentedString))
          println(" \n \n" + str.toStringColorized + "\n \n ")

          assertTrue(
            schema.discriminator.contains("type"),
            aFields.map(_.name) == List(),
            bFields.map(_.name) == List(),
            cFields.map(_.name) == List("p2"),
            cFields.find(_.name == "p2").exists(_.schema == JsonSchema[Product2]),
          )
        },
      ),
    )

}
