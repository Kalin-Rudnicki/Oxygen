package oxygen.transform

import oxygen.core.model.ScopePath
import oxygen.predef.test.*

object TransformOrFailSpec extends OxygenSpecDefault {

  private def makeSuccess[From, To](name: String)(from: From, to: To)(using TransformOrFail[From, To]): TestSpec =
    test(name) {
      assertTrue(from.transformIntoOrFail[To] == to.asRight)
    }

  private def makeFailure[From, To](name: String)(from: From, expected: TransformError)(using TransformOrFail[From, To]): TestSpec =
    test(name) {
      assertTrue(from.transformIntoOrFail[To] == expected.asLeft)
    }

  //////// models ////////

  final case class WrappedString(s: String)
  final case class PositiveInt(value: Int)

  final case class PartialPerson(
      first: String,
      last: Option[String],
      age: Option[Int],
  )
  final case class FullPerson(
      first: String,
      last: String,
      age: Int,
  )

  enum PartialSum {
    case A(s: String, flag: Option[Boolean])
    case B(n: Option[Int])
  }
  enum FullSum {
    case A(s: String, flag: Boolean)
    case B(n: Int)
  }

  //////// instances ////////

  given Transform[WrappedString, String] = _.s

  given TransformOrFail[Int, PositiveInt] = TransformOrFail.fromEitherF { i =>
    if i > 0 then PositiveInt(i).asRight
    else s"expected positive, got $i".asLeft
  }

  given TransformOrFail[PartialPerson, FullPerson] = TransformOrFail.derived
  given TransformOrFail[PartialSum, FullSum] = TransformOrFail.derived

  //////// tests ////////

  override def testSpec: TestSpec =
    suite("TransformOrFailSpec")(
      suite("givens")(
        makeSuccess("from infallible Transform")(WrappedString("A"), "A"),
        makeSuccess("id - String")("A", "A"),
        makeSuccess("seq")(List("A", "B", "C"), Vector("A", "B", "C")),
        makeSuccess("seq - WrappedString")(List(WrappedString("A"), WrappedString("B")), Vector("A", "B")),
        makeSuccess("option - some")("A".some, "A".some),
        makeSuccess("option - none")(Option.empty[String], Option.empty[String]),
        makeSuccess("pure - some")("A", "A".some),
        // the new case: Option[A] -> B (require present)
        makeSuccess("requireOption - some")("A".some, "A"),
        makeFailure[Option[String], String]("requireOption - none")(
          Option.empty[String],
          TransformError(Nil, TransformError.Cause.MissingRequired),
        ),
        makeSuccess("requireOption - WrappedString")(WrappedString("A").some, "A"),
        // DecodeString via StringDecoder
        makeSuccess("decodeString - Int")("42", 42),
        makeSuccess("decodeString - Boolean")("true", true),
        makeFailure[String, Int]("decodeString - Int fail")(
          "nope",
          TransformError(
            Nil,
            TransformError.Cause.DecodingFailure(StringDecoder[Int].decodeDetailed("nope").swap.toOption.get),
          ),
        ),
        makeFailure[String, Boolean]("decodeString - Boolean fail")(
          "nope",
          TransformError(
            Nil,
            TransformError.Cause.DecodingFailure(StringDecoder[Boolean].decodeDetailed("nope").swap.toOption.get),
          ),
        ),
        makeSuccess("decodeString - seq of Int")(List("1", "2", "3"), Vector(1, 2, 3)),
        makeFailure[List[String], Vector[Int]]("decodeString - seq fail at index")(
          List("1", "nope", "3"),
          TransformError(
            List(ScopePath.Index(1)),
            TransformError.Cause.DecodingFailure(StringDecoder[Int].decodeDetailed("nope").swap.toOption.get),
          ),
        ),
      ),
      suite("fallible instance")(
        makeSuccess("PositiveInt - ok")(5, PositiveInt(5)),
        makeFailure[Int, PositiveInt]("PositiveInt - fail")(
          0,
          TransformError(Nil, TransformError.Cause.DecodingFailure("expected positive, got 0")),
        ),
        makeSuccess("seq of PositiveInt - ok")(List(1, 2, 3), Vector(PositiveInt(1), PositiveInt(2), PositiveInt(3))),
        makeFailure[List[Int], Vector[PositiveInt]]("seq of PositiveInt - fail at index")(
          List(1, -2, 3),
          TransformError(List(ScopePath.Index(1)), TransformError.Cause.DecodingFailure("expected positive, got -2")),
        ),
        makeSuccess("option of PositiveInt - some")(5.some, PositiveInt(5).some),
        makeSuccess("option of PositiveInt - none")(Option.empty[Int], Option.empty[PositiveInt]),
        makeFailure[Option[Int], Option[PositiveInt]]("option of PositiveInt - fail")(
          (-1).some,
          TransformError(Nil, TransformError.Cause.DecodingFailure("expected positive, got -1")),
        ),
        makeSuccess("requireOption PositiveInt - some")(5.some, PositiveInt(5)),
        makeFailure[Option[Int], PositiveInt]("requireOption PositiveInt - none")(
          Option.empty[Int],
          TransformError(Nil, TransformError.Cause.MissingRequired),
        ),
        makeFailure[Option[Int], PositiveInt]("requireOption PositiveInt - present but invalid")(
          0.some,
          TransformError(Nil, TransformError.Cause.DecodingFailure("expected positive, got 0")),
        ),
      ),
      suite("derived product")(
        makeSuccess("PartialPerson -> FullPerson - all present")(
          PartialPerson("F", "L".some, 30.some),
          FullPerson("F", "L", 30),
        ),
        makeFailure[PartialPerson, FullPerson]("PartialPerson -> FullPerson - missing last")(
          PartialPerson("F", None, 30.some),
          TransformError(List(ScopePath.Field("last")), TransformError.Cause.MissingRequired),
        ),
        makeFailure[PartialPerson, FullPerson]("PartialPerson -> FullPerson - missing age")(
          PartialPerson("F", "L".some, None),
          TransformError(List(ScopePath.Field("age")), TransformError.Cause.MissingRequired),
        ),
      ),
      suite("derived sum")(
        makeSuccess("PartialSum.A -> FullSum.A")(
          PartialSum.A("str", true.some),
          FullSum.A("str", true),
        ),
        makeSuccess("PartialSum.B -> FullSum.B")(
          PartialSum.B(5.some),
          FullSum.B(5),
        ),
        makeFailure[PartialSum, FullSum]("PartialSum.A - missing flag")(
          PartialSum.A("str", None),
          TransformError(
            List(ScopePath.SubType("A"), ScopePath.Field("flag")),
            TransformError.Cause.MissingRequired,
          ),
        ),
        makeFailure[PartialSum, FullSum]("PartialSum.B - missing n")(
          PartialSum.B(None),
          TransformError(
            List(ScopePath.SubType("B"), ScopePath.Field("n")),
            TransformError.Cause.MissingRequired,
          ),
        ),
      ),
      suite("existing domain/api via derived OrFail")(
        {
          given Transform[domain.Email, String] = _.email
          given Transform[domain.Password, String] = _.password
          given TransformOrFail[domain.Person, api.Person] = TransformOrFail.derived
          given TransformOrFail[domain.SumExample, api.SumExample] = TransformOrFail.derived

          suite("lifted from Transform")(
            makeSuccess("domain.Person -> api.Person")(
              domain.Person("F", "L", domain.Email("email@e.mail"), domain.Password("pass")),
              api.Person("F", "L", "email@e.mail", "pass"),
            ),
            makeSuccess[domain.SumExample, api.SumExample]("domain.SumExample.A")(
              domain.SumExample.A("str", true, 5),
              api.SumExample.A("str", true.some),
            ),
            makeSuccess[domain.SumExample, api.SumExample]("domain.SumExample.B")(
              domain.SumExample.B(5),
              api.SumExample.B,
            ),
          )
        },
      ),
    )

}
