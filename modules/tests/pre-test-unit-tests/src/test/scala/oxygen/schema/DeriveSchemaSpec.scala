package oxygen.schema

import java.time.*
import oxygen.json.*
import oxygen.json.syntax.json.*
import oxygen.predef.test.*
import oxygen.schema.compiled.*
import zio.test.TestResult

object DeriveSchemaSpec extends OxygenSpecDefault {

  final case class Product1(
      field1: String,
      field2: Option[Boolean],
  ) derives JsonSchema.ObjectLike

  final case class Product2(
      @jsonField("product") p: Product1,
  ) derives JsonSchema.ObjectLike
  object Product2 {

    final case class Wrapped(value: Product2)
    object Wrapped {
      given JsonSchema[Wrapped] = JsonSchema.deriveWrapped[Wrapped]
    }

    final case class OptWrapped(value: Option[Product2])
    object OptWrapped {
      given JsonSchema[OptWrapped] = JsonSchema.deriveWrapped[OptWrapped]
    }

  }

  final case class Product3(
      value: String,
      rec: Option[Product3],
      timestamp: Instant,
      date: LocalDate,
      updated: Updated,
  ) derives JsonSchema.ObjectLike

  final case class Updated(at: Option[Instant])
  object Updated {
    given JsonSchema[Updated] = JsonSchema.option[Instant].transform(Updated(_), _.at)
  }

  enum MyEnum derives StrictEnum { case A, B, C }

  enum Sum1 derives JsonSchema.ObjectLike {
    @jsonType("_A_") case A
    case B()
    case C(@jsonField("p2") p: Product2)
  }

  @jsonDiscriminator("type")
  enum Sum2 derives JsonSchema.ObjectLike {
    case A
    @jsonType("_B_") case B()
    case C(@jsonField("p2") p: Product2, p3: Product3, e: MyEnum)
  }

  final case class IntPlusOne(i: Int)
  object IntPlusOne {
    given JsonSchema[IntPlusOne] = JsonSchema.int.transform(_ + 1, _ - 1).transform(_ - 1, _ + 1).transform(_ + 1, _ - 1).transformAuto[IntPlusOne]
  }

  final case class Base64OxygenDate(product1: LocalDate)
  object Base64OxygenDate {
    import JsonSchema.oxygenTime.given
    given PlainTextSchema[Base64OxygenDate] = JsonSchema.deriveWrapped[Base64OxygenDate] @@ PlainTextSchema.Encoding.base64WithPadding
    given JsonSchema[Base64OxygenDate] = JsonSchema.fromPlainText
  }

  final case class FlattenOuterRequired(
      outer1: Int,
      outer2: Option[String],
      @jsonFlatten inner: FlattenInner,
  ) derives JsonSchema

  final case class FlattenInner(
      inner1: Int,
      inner2: Option[String],
  ) derives JsonSchema

  final case class TestProduct(
      ip1: IntPlusOne,
      b64p1: Base64OxygenDate,
      int1: Int,
      int2: Option[Int],
      int3: Specified[Int],
      int4: Specified[Option[Int]],
      int5: Option[Specified[Int]],
      wrapped1: Updated,
      wrapped2: Option[Updated],
      wrapped3: Specified[Updated],
      wrapped4: Specified[Option[Updated]],
      wrapped5: Option[Specified[Updated]],
  ) derives JsonSchema

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
            cFields.map(_.name) == List("p2", "p3", "e"),
            cFields.find(_.name == "p2").exists(_.schema == JsonSchema[Product2]),
          )
        },
      ),
      suite("FlattenOuterRequired")(
        test("works") {
          val schema: JsonSchema.ProductSchema[FlattenOuterRequired] = JsonSchema[FlattenOuterRequired].asInstanceOf[JsonSchema.ProductSchema[FlattenOuterRequired]]
          val fields = schema.fields.toList
          assertTrue(
            fields.map(_.name) == List("outer1", "outer2", "inner1", "inner2"),
            fields.find(_.name == "outer1").exists(_.schema == JsonSchema.int),
          )
        },
      ),
      test("compiler") {
        val compiling: Compiled[CompiledSchemaRef] =
          for {
            _ <- Compiled.usingJson[Product1]
            _ <- Compiled.usingJson[Product3]
            _ <- Compiled.usingJson[Sum1]
            _ <- Compiled.usingJson[Sum2]
            _ <- Compiled.usingJson[Option[Product2]]
            _ <- Compiled.usingJson[List[Product2]]
            _ <- Compiled.plain(PlainTextSchema.bearerToken)
            _ <- Compiled.plain(PlainTextSchema.standardJWT[Product1])
            _ <- Compiled.usingJson[Product2.Wrapped]
            _ <- Compiled.usingJson[Product2.OptWrapped]
            _ <- Compiled.usingJson[FlattenOuterRequired]
            ref <- Compiled.usingJson[TestProduct]
          } yield ref

        val rawCompiled = compiling.compiled.schemas
        val fullCompiled = FullCompiledSchemas(rawCompiled)

        val exportFiles: Boolean = true

        def showTypes: IndentedString =
          IndentedString.inline(
            IndentedString.Break,
            IndentedString.section("raw:")(
              rawCompiled.all.map(s => s"- ${s.ref.showCore}"),
            ),
            IndentedString.Break,
            IndentedString.section("full:")(
              fullCompiled.allSchemas.map(s => s"- ${s.ref.showCore}"),
            ),
            IndentedString.Break,
          )

        val doExport: Task[Unit] =
          ZIO.writeFile("target/raw-compiled.json", rawCompiled.toJsonStringPretty) <&>
            ZIO.writeFile("target/types.yaml", showTypes.toString("  ")) <&>
            ZIO.writeFile("target/full-compiled.yaml", fullCompiled.allSchemas.map(_.toIndentedString.toString("  ")).mkString("\n", "\n\n", "\n"))

        doExport.whenDiscard(exportFiles) *>
          assertCompletesZIO
      },
    )

}
