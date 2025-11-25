package oxygen.schema

import java.time.LocalDate
import oxygen.predef.test.*
import oxygen.schema.compat.*
import oxygen.schema.compiled.*

object CompatSpec extends OxygenSpecDefault {

  private object person {

    final case class NoAge(first: String, last: String) derives JsonSchema
    final case class RequiredAge(first: String, last: String, age: Int) derives JsonSchema
    final case class OptionalAge(first: String, last: String, age: Option[Int]) derives JsonSchema
    final case class SpecifiedAge(first: String, last: String, age: Specified[Int], loop: Option[AgeDecimal]) derives JsonSchema
    final case class SpecifiedOptionalAge(first: String, last: String, age: Specified[Option[Int]]) derives JsonSchema
    final case class AgeBirthday(first: String, last: String, age: Specified[LocalDate]) derives JsonSchema
    final case class AgeDecimal(first: String, last: String, age: Option[Double], loop: Option[AgeDecimal]) derives JsonSchema

  }

  private final case class CompileResult(
      person: CompileResult.Person,
  )
  private object CompileResult {

    final case class Person(
        noAge: CompiledSchemaRef,
        requiredAge: CompiledSchemaRef,
        optionalAge: CompiledSchemaRef,
        specifiedAge: CompiledSchemaRef,
        specifiedOptionalAge: CompiledSchemaRef,
        ageBirthday: CompiledSchemaRef,
        ageDecimal: CompiledSchemaRef,
    )
    object Person {

      val compile: Compiled[CompileResult.Person] =
        for {
          noAge <- Compiled.usingJson[CompatSpec.person.NoAge]
          requiredAge <- Compiled.usingJson[CompatSpec.person.RequiredAge]
          optionalAge <- Compiled.usingJson[CompatSpec.person.OptionalAge]
          specifiedAge <- Compiled.usingJson[CompatSpec.person.SpecifiedAge]
          specifiedOptionalAge <- Compiled.usingJson[CompatSpec.person.SpecifiedOptionalAge]
          ageBirthday <- Compiled.usingJson[CompatSpec.person.AgeBirthday]
          ageDecimal <- Compiled.usingJson[CompatSpec.person.AgeDecimal]
        } yield CompileResult.Person(
          noAge = noAge,
          requiredAge = requiredAge,
          optionalAge = optionalAge,
          specifiedAge = specifiedAge,
          specifiedOptionalAge = specifiedOptionalAge,
          ageBirthday = ageBirthday,
          ageDecimal = ageDecimal,
        )

    }

    val compile: Compiled[CompileResult] =
      for {
        person <- Person.compile
      } yield CompileResult(
        person = person,
      )

    val output: Compiled.Output[CompileResult] = compile.compiled

  }

  val personRefs: Seq[CompiledSchemaRef] = {
    val p = CompileResult.output.value.person
    Seq(
      p.noAge,
      p.requiredAge,
      // p.optionalAge,
      p.specifiedAge,
      // p.specifiedOptionalAge,
      p.ageBirthday,
      p.ageDecimal,
    )
  }

  val personRefPairs0: Seq[(CompiledSchemaRef, CompiledSchemaRef)] =
    for {
      from <- personRefs
      to <- personRefs
    } yield (from, to)

  val personRefPairs: Seq[(CompiledSchemaRef, CompiledSchemaRef)] = {
    val p = CompileResult.output.value.person
    Seq(
      p.noAge -> p.noAge,
      p.noAge -> p.requiredAge,
      p.requiredAge -> p.requiredAge,
      p.requiredAge -> p.noAge,
      p.optionalAge -> p.ageBirthday,
      p.optionalAge -> p.ageDecimal,
      p.specifiedAge -> p.ageDecimal,
    )
  }

  private val compared: Compared[Seq[ComparisonResult.Concrete]] =
    Compared.traverse(personRefPairs) { Compared.compareRoot(_, _) }

  private val full = CompileResult.output.schemas.toFullCompiledSchemas

  private val res: Seq[ComparisonResult.Concrete] = compared.eval(full, full).result

  // TODO (KR) : do a real test
  res.zipWithIndex.foreach { case (res, idx) =>
    println(
      s"""
         |----- ${idx + 1} ---------------------------------------------------------------------------------------------
         |
         |from: ${res.from.ref.showCore}
         |to: ${res.to.ref.showCore}
         |
         |${("\n" + res.toIndentedStringNoTypes.toStringColorized).replaceAll("\n", "\n    ")}
         |
         |${("\n" + res.pruned.toIndentedStringNoTypes.toStringColorized).replaceAll("\n", "\n    ")}
         |""".stripMargin,
    )
  }

  override def testSpec: TestSpec =
    suite("CompatSpec")(
      // TODO (KR) :
    )

}
