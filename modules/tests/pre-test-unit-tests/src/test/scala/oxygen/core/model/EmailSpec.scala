package oxygen.core.model

import oxygen.predef.test.*

object EmailSpec extends OxygenSpecDefault {

  private def parse(raw: String): Option[(String, Option[String], String)] =
    Email.fromString(raw).map(e => (e.username, e.tag, e.domain))

  private val parsingSpec: TestSpec =
    suite("parsing")(
      test("parses a bare address") {
        assertTrue(parse("john@example.com").contains(("john", None, "example.com")))
      },
      test("parses a '+' tag (default separator)") {
        assertTrue(
          parse("john+work@gmail.com").contains(("john", Some("work"), "gmail.com")),
          parse("john+shopping@outlook.com").contains(("john", Some("shopping"), "outlook.com")),
        )
      },
      test("keeps a '+' tag boundary at the first '+' only, tag may contain more '+'") {
        assertTrue(parse("john+a+b@gmail.com").contains(("john", Some("a+b"), "gmail.com")))
      },
      test("uses '-' as the tag separator for the Yahoo family") {
        assertTrue(
          parse("john-work@yahoo.com").contains(("john", Some("work"), "yahoo.com")),
          parse("john-work@ymail.com").contains(("john", Some("work"), "ymail.com")),
          parse("john-work@rocketmail.com").contains(("john", Some("work"), "rocketmail.com")),
        )
      },
      test("a '-' in a '+'-separator domain stays part of the username") {
        assertTrue(parse("mary-jane@gmail.com").contains(("mary-jane", None, "gmail.com")))
      },
      test("a '+' in a '-'-separator domain stays part of the username") {
        assertTrue(parse("john+work@yahoo.com").contains(("john+work", None, "yahoo.com")))
      },
      test("separator lookup is case-insensitive on the domain") {
        assertTrue(parse("john-work@YAHOO.COM").contains(("john", Some("work"), "YAHOO.COM")))
      },
      test("preserves case at parse time (normalization is a separate step)") {
        assertTrue(parse("John.Doe@Example.COM").contains(("John.Doe", None, "Example.COM")))
      },
      test("allows dots, underscores, hyphens, and percent in the username") {
        assertTrue(parse("a.b_c-d%e@example.com").contains(("a.b_c-d%e", None, "example.com")))
      },
      test("accepts multi-level domains") {
        assertTrue(parse("john@mail.co.uk").contains(("john", None, "mail.co.uk")))
      },
      test("rejects malformed addresses") {
        assertTrue(
          Email.fromString("").isEmpty,
          Email.fromString("john").isEmpty,
          Email.fromString("john@").isEmpty,
          Email.fromString("@example.com").isEmpty,
          Email.fromString("john@example").isEmpty, // no TLD
          Email.fromString("john@@example.com").isEmpty, // double @
          Email.fromString("a@b@example.com").isEmpty, // two @
          Email.fromString("john doe@example.com").isEmpty, // space
        )
      },
      test("rejects a leading separator (empty username)") {
        assertTrue(
          Email.fromString("+tag@gmail.com").isEmpty,
          Email.fromString("-tag@yahoo.com").isEmpty,
        )
      },
      test("rejects a trailing separator (empty tag)") {
        assertTrue(
          Email.fromString("john+@gmail.com").isEmpty,
          Email.fromString("john-@yahoo.com").isEmpty,
        )
      },
      test("captures the separator alongside the tag in the parse result") {
        assertTrue(
          Email.fromString("john+work@gmail.com").flatMap(_.subaddress).map(_.separator).contains('+'),
          Email.fromString("john-work@yahoo.com").flatMap(_.subaddress).map(_.separator).contains('-'),
          Email.fromString("john+work@gmail.com").flatMap(_.subaddress).map(_.tag).contains("work"),
          Email.fromString("john@example.com").flatMap(_.subaddress).isEmpty,
        )
      },
      test("round-trips through show for every separator") {
        assertTrue(
          Email.fromString("john@example.com").map(_.toString).contains("john@example.com"),
          Email.fromString("john+work@gmail.com").map(_.toString).contains("john+work@gmail.com"),
          Email.fromString("john-work@yahoo.com").map(_.toString).contains("john-work@yahoo.com"),
        )
      },
      test("unsafe throws on an invalid address and returns on a valid one") {
        assertTrue(
          Email.unsafe("john+work@gmail.com").toString == "john+work@gmail.com",
          scala.util.Try(Email.unsafe("nope")).isFailure,
        )
      },
    )

  private def normalized(raw: String, stripDots: Email.StripDots): Option[String] =
    Email.fromString(raw).map(_.normalize(stripDots).toString)

  private def normalizedAuto(raw: String): Option[String] =
    Email.fromString(raw).map(_.normalize.toString)

  private val normalizationSpec: TestSpec =
    suite("normalization")(
      test("lowercases username and domain in all modes") {
        assertTrue(
          normalized("John.Doe@Example.COM", Email.StripDots.No).contains("john.doe@example.com"),
          normalized("John.Doe@Example.COM", Email.StripDots.Yes).contains("johndoe@example.com"),
          normalized("John.Doe@Example.COM", Email.StripDots.Auto).contains("john.doe@example.com"),
        )
      },
      test("always drops the tag") {
        assertTrue(
          normalizedAuto("john+work@gmail.com").contains("john@gmail.com"),
          normalizedAuto("john-work@yahoo.com").contains("john@yahoo.com"),
          normalized("john+work@example.com", Email.StripDots.No).contains("john@example.com"),
        )
      },
      suite("StripDots.Auto")(
        test("strips dots for Gmail-family domains") {
          assertTrue(
            normalizedAuto("john.doe@gmail.com").contains("johndoe@gmail.com"),
            normalizedAuto("john.doe@googlemail.com").contains("johndoe@googlemail.com"),
            normalizedAuto("John.Doe+promo@GMAIL.com").contains("johndoe@gmail.com"),
          )
        },
        test("keeps dots for providers that treat them as significant") {
          assertTrue(
            normalizedAuto("john.doe@outlook.com").contains("john.doe@outlook.com"),
            normalizedAuto("john.doe@yahoo.com").contains("john.doe@yahoo.com"),
            normalizedAuto("john.doe@proton.me").contains("john.doe@proton.me"),
            normalizedAuto("john.doe@example.com").contains("john.doe@example.com"),
          )
        },
      ),
      suite("StripDots.Yes")(
        test("strips dots regardless of domain") {
          assertTrue(
            normalized("john.doe@gmail.com", Email.StripDots.Yes).contains("johndoe@gmail.com"),
            normalized("john.doe@outlook.com", Email.StripDots.Yes).contains("johndoe@outlook.com"),
            normalized("j.o.h.n@example.com", Email.StripDots.Yes).contains("john@example.com"),
          )
        },
      ),
      suite("StripDots.No")(
        test("never strips dots, even for Gmail") {
          assertTrue(
            normalized("john.doe@gmail.com", Email.StripDots.No).contains("john.doe@gmail.com"),
            normalized("john.doe@outlook.com", Email.StripDots.No).contains("john.doe@outlook.com"),
          )
        },
      ),
      test("normalization is idempotent") {
        assertTrue(
          List("john.doe+tag@gmail.com", "john-tag@yahoo.com", "A.B@Example.com").forall { raw =>
            val once = Email.fromString(raw).get.normalize
            once.normalize == once
          },
        )
      },
    )

  override def testSpec: TestSpec =
    suite("EmailSpec")(
      parsingSpec,
      normalizationSpec,
    )

}
