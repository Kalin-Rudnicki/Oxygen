package oxygen.schema

import java.util.UUID
import oxygen.core.{PlatformCompat, SourcePosition}
import oxygen.meta.k0.*
import oxygen.predef.core.*
import scala.util.Try

trait SchemaLike[A] { self =>

  private[schema] final val schemaId: UUID = PlatformCompat.randomUUID() // used in the detection of recursive schemas

  protected final def withHeader(header: String, tt: TypeTag[?], args: (String, String)*): String = s"$header[${tt.prefixAll}](${args.map { case (k, v) => s"$k=$v" }.mkString(",")})"
  protected final def withHeader(header: String, args: (String, String)*): String = withHeader(header, typeTag, args*)
  protected def __internalReferenceOf(builder: SchemaLike.ReferenceBuilder): String

  private[schema] final lazy val referenceName: SchemaLike.ReferenceName = SchemaLike.ReferenceName(SchemaLike.ReferenceBuilder(Map.empty).referenceOf(this))

  type S[a] <: SchemaLike[a]

  val typeTag: TypeTag[A]

  def decode(string: String): Either[String, A]
  def encode(value: A): String

  def transform[B: TypeTag as newTypeTag](ab: A => B, ba: B => A)(using pos: SourcePosition): S[B]
  def transformOrFail[B: TypeTag as newTypeTag](ab: A => Either[String, B], ba: B => A)(using pos: SourcePosition): S[B]

  final def transformOption[B: TypeTag as newTypeTag](ab: A => Option[B], ba: B => A)(using pos: SourcePosition): S[B] =
    transformOrFail(a => ab(a).toRight(s"Invalid '${newTypeTag.prefixObject}': $a"), ba)
  final def transformAttempt[B: TypeTag as newTypeTag](ab: A => B, ba: B => A)(using pos: SourcePosition): S[B] =
    transformOption(a => Try { ab(a) }.toOption, ba)

  final def transformOptionObscure[B: TypeTag as newTypeTag](ab: A => Option[B], ba: B => A)(using pos: SourcePosition): S[B] =
    transformOrFail(a => ab(a).toRight("Unable to decode invalid value"), ba)
  final def transformAttemptObscure[B: TypeTag as newTypeTag](ab: A => B, ba: B => A): S[B] =
    transformOptionObscure(a => Try { ab(a) }.toOption, ba)

  inline final def transformAuto[B: TypeTag](using pos: SourcePosition): S[B] = {
    val (ab, ba) = ProductGeneric.deriveTransform[A, B]
    transform[B](ab, ba)
  }

  /**
    * There are times when you only care about encoding or decoding, and not the other.
    * This makes that possible.
    * If you try to do the thing you explicitly decided not to implement, you will get a runtime exception.
    */
  object unsafe {

    def map[B: TypeTag as newTypeTag](ab: A => B)(using pos: SourcePosition): S[B] =
      transform(ab, _ => throw SchemaLike.UnsafeEncodingFailure(self.typeTag, newTypeTag))

    def mapOrFail[B: TypeTag as newTypeTag](ab: A => Either[String, B])(using pos: SourcePosition): S[B] =
      transformOrFail(ab, _ => throw SchemaLike.UnsafeEncodingFailure(self.typeTag, newTypeTag))

    def mapOption[B: TypeTag as newTypeTag](ab: A => Option[B])(using pos: SourcePosition): S[B] =
      transformOrFail(a => ab(a).toRight(s"Invalid '${newTypeTag.prefixObject}': $a"), _ => throw SchemaLike.UnsafeEncodingFailure(self.typeTag, newTypeTag))
    def mapAttempt[B: TypeTag as newTypeTag](ab: A => B): S[B] =
      mapOption(a => Try { ab(a) }.toOption)

    def mapOptionObscure[B: TypeTag as newTypeTag](ab: A => Option[B])(using pos: SourcePosition): S[B] =
      transformOrFail(a => ab(a).toRight("Unable to decode invalid value"), _ => throw SchemaLike.UnsafeEncodingFailure(self.typeTag, newTypeTag))
    def mapAttemptObscure[B: TypeTag as newTypeTag](ab: A => B)(using pos: SourcePosition): S[B] =
      mapOptionObscure(a => Try { ab(a) }.toOption)

    def contramap[B: TypeTag as newTypeTag](ba: B => A)(using pos: SourcePosition): S[B] =
      transform(_ => throw SchemaLike.UnsafeDecodingFailure(self.typeTag, newTypeTag), ba)

  }

}
object SchemaLike {

  private[schema] final class ReferenceBuilder private[SchemaLike] (seenFullTypeNames: Map[UUID, String]) {

    def referenceOf(schema: SchemaLike[?]): String =
      seenFullTypeNames.get(schema.schemaId) match
        case Some(seenFullTypeName) => s"RecursiveReference<$seenFullTypeName>"
        case None                   => schema.__internalReferenceOf(ReferenceBuilder(seenFullTypeNames.updated(schema.schemaId, schema.typeTag.prefixAll)))

  }

  private[schema] final case class ReferenceName private[SchemaLike] (reference: String) {

    override val hashCode: Int = reference.hashCode

    override def equals(that: Any): Boolean = that.asInstanceOf[Matchable] match
      case that: ReferenceName => this.reference == that.reference
      case _                   => false

  }
  private[schema] object ReferenceName {
    given Ordering[ReferenceName] = Ordering.by[ReferenceName, Int](_.reference.length).orElseBy(_.reference)
  }

  // TODO (KR) : add SourceLocation
  final case class UnsafeEncodingFailure(aTag: TypeTag[?], bTag: TypeTag[?]) extends Throwable {
    override def getMessage: String =
      s"""The author of this schema wants you to know that they have a personal vendetta against you and your entire family.
         |Just kidding...
         |
         |This schema was implemented in a way where decoding from ( ${aTag.prefixObject} => ${bTag.prefixObject} ) is supported,
         |but encoding back from ( ${bTag.prefixObject} => ${aTag.prefixObject} ) is not supported.
         |
         |Your options at this point are to implement encoding in this schema (if you own it), or you will have to create your own...""".stripMargin
    override def toString: String = getMessage
  }

  // TODO (KR) : add SourceLocation
  final case class UnsafeDecodingFailure(aTag: TypeTag[?], bTag: TypeTag[?]) extends Throwable {
    override def getMessage: String =
      s"""The author of this schema wants you to know that they have a personal vendetta against you and your entire family.
         |Just kidding...
         |
         |This schema was implemented in a way where encoding from ( ${bTag.prefixObject} => ${aTag.prefixObject} ) is supported,
         |but decoding back from ( ${aTag.prefixObject} => ${bTag.prefixObject} ) is not supported.
         |
         |Your options at this point are to implement decoding in this schema (if you own it), or you will have to create your own...""".stripMargin
    override def toString: String = getMessage
  }

}
