package oxygen.core

import oxygen.predef.core.*
import oxygen.test.*
import oxygen.test.OAssertions.*
import scala.reflect.ClassTag
import zio.test.*
import zio.test.Assertion.*

object TypeTagSpec extends OxygenSpecDefault {

  object types {

    object Companion

    final class NonGeneric
    final class Generic[D, E, F]

    final class Stage1[A] {
      final class Stage2[B] {
        final class Stage3[C]
      }
    }

  }

  private def classTagTest[A: ClassTag](exp: TypeTag.TypeRef.Single): TestSpec =
    test(exp.polyShow(_.prefixAll)) {
      assert(TypeTag.usingClassTag[A].tag)(equalTo(exp))
    }

  private inline def deriveTagTest[A](exp: TypeTag.TypeRef): TestSpec =
    test(exp.polyShow(_.prefixAll)) {
      assert(TypeTag.derived[A].tag)(equalTo_filteredDiff(exp))
    }

  private val typeTagSpecRef: TypeTag.TypeRef.Single = TypeTag.TypeRef.Single.make("oxygen", "core")()("TypeTagSpec")
  private val typesRef: TypeTag.TypeRef.Single = TypeTag.TypeRef.Single("types", Nil, typeTagSpecRef.asLeft)

  private def stage1Ref(a: TypeTag.TypeRef): TypeTag.TypeRef.Single =
    TypeTag.TypeRef.Single("Stage1", a :: Nil, typesRef.asLeft)
  private def stage2Ref(a: TypeTag.TypeRef, b: TypeTag.TypeRef): TypeTag.TypeRef.Single =
    TypeTag.TypeRef.Single("Stage2", b :: Nil, stage1Ref(a).asLeft)
  private def stage3Ref(a: TypeTag.TypeRef, b: TypeTag.TypeRef, c: TypeTag.TypeRef): TypeTag.TypeRef.Single =
    TypeTag.TypeRef.Single("Stage3", c :: Nil, stage2Ref(a, b).asLeft)

  private def optionTag[A: TypeTag]: TypeTag[Option[A]] =
    TypeTag.derived

  override def testSpec: TestSpec =
    suite("TypeTagSpec")(
      suite("from:")(
        test("make") {
          assert(TypeTag.TypeRef.Single.make("a", "b", "c")("d", "e", "f")("g"))(
            equalTo(
              TypeTag.TypeRef.Single(
                "g",
                Nil,
                TypeTag.TypeRef
                  .Single(
                    "f",
                    Nil,
                    TypeTag.TypeRef
                      .Single(
                        "e",
                        Nil,
                        TypeTag.TypeRef
                          .Single(
                            "d",
                            Nil,
                            List("a", "b", "c").asRight,
                          )
                          .asLeft,
                      )
                      .asLeft,
                  )
                  .asLeft,
              ),
            ),
          )
        },
        suite("classTag")(
          classTagTest[Int](TypeTag.TypeRef.Single.make()()("int")),
          classTagTest[Boolean](TypeTag.TypeRef.Single.make()()("boolean")),
          classTagTest[String](TypeTag.TypeRef.Single.make("java", "lang")()("String")),
          classTagTest[types.Companion.type](TypeTag.TypeRef.Single.make("oxygen", "core")("TypeTagSpec", "types")("Companion")),
          classTagTest[types.NonGeneric](TypeTag.TypeRef.Single.make("oxygen", "core")("TypeTagSpec", "types")("NonGeneric")),
          classTagTest[types.Generic[Int, Boolean, String]](
            TypeTag.TypeRef.Single
              .make("oxygen", "core")("TypeTagSpec", "types")("Generic")
              .withTypeArgs(
                TypeTag.TypeRef.Single.make()()("D"),
                TypeTag.TypeRef.Single.make()()("E"),
                TypeTag.TypeRef.Single.make()()("F"),
              ),
          ),
          classTagTest[types.Stage1[Int]](
            TypeTag.TypeRef.Single
              .make("oxygen", "core")("TypeTagSpec", "types")("Stage1")
              .withTypeArgs(
                TypeTag.TypeRef.Single.make()()("A"),
              ),
          ),
          classTagTest[types.Stage1[Int]#Stage2[String]](
            TypeTag.TypeRef.Single
              .make("oxygen", "core")("TypeTagSpec", "types", "Stage1")("Stage2")
              .withTypeArgs(
                TypeTag.TypeRef.Single.make()()("B"),
              ),
          ),
          classTagTest[types.Stage1[Int]#Stage2[String]#Stage3[Boolean]](
            TypeTag.TypeRef.Single
              .make("oxygen", "core")("TypeTagSpec", "types", "Stage1", "Stage2")("Stage3")
              .withTypeArgs(
                TypeTag.TypeRef.Single.make()()("C"),
              ),
          ),
        ),
        suite("derived")(
          deriveTagTest[Int](TypeTag.TypeRef.Single.make("scala")()("Int")),
          deriveTagTest[Boolean](TypeTag.TypeRef.Single.make("scala")()("Boolean")),
          deriveTagTest[String](TypeTag.TypeRef.Single.make("java", "lang")()("String")),
          deriveTagTest[types.Companion.type](TypeTag.TypeRef.Single.make("oxygen", "core")("TypeTagSpec", "types")("Companion")),
          deriveTagTest[types.NonGeneric](TypeTag.TypeRef.Single.make("oxygen", "core")("TypeTagSpec", "types")("NonGeneric")),
          deriveTagTest[types.Generic[Int, Boolean, String]](
            TypeTag.TypeRef.Single
              .make("oxygen", "core")("TypeTagSpec", "types")("Generic")
              .withTypeArgs(
                TypeTag.TypeRef.Single.make("scala")()("Int"),
                TypeTag.TypeRef.Single.make("scala")()("Boolean"),
                TypeTag.TypeRef.Single.make("java", "lang")()("String"),
              ),
          ),
          deriveTagTest[types.Stage1[Int]](
            stage1Ref(
              TypeTag.TypeRef.Single.make("scala")()("Int"),
            ),
          ),
          deriveTagTest[types.Stage1[Int]#Stage2[String]](
            stage2Ref(
              TypeTag.TypeRef.Single.make("scala")()("Int"),
              TypeTag.TypeRef.Single.make("java", "lang")()("String"),
            ),
          ),
          deriveTagTest[types.Stage1[Int]#Stage2[String]#Stage3[Boolean]](
            stage3Ref(
              TypeTag.TypeRef.Single.make("scala")()("Int"),
              TypeTag.TypeRef.Single.make("java", "lang")()("String"),
              TypeTag.TypeRef.Single.make("scala")()("Boolean"),
            ),
          ),
          test("derives param correctly") {
            assert(optionTag[Int].tag)(
              equalTo_filteredDiff(
                TypeTag.TypeRef.Single
                  .make("scala")()("Option")
                  .withTypeArgs(
                    TypeTag.TypeRef.Single.make("scala")()("Int"),
                  ),
              ),
            )
          },
        ),
      ),
    )

}
