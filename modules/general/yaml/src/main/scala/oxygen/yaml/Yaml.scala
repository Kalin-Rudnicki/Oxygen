package oxygen.yaml

import oxygen.predef.core.*

sealed trait Yaml {
  override final def toString: String = YamlBuilder.format(this)
}
object Yaml {

  sealed trait NonEmpty extends Yaml

  case object Empty extends Yaml

  case object Null extends Yaml.NonEmpty

  final case class Bool(value: Boolean) extends Yaml.NonEmpty

  sealed trait Str extends Yaml.NonEmpty

  final case class InlineStr(value: String) extends Yaml.Str

  final case class Num(value: BigDecimal) extends Yaml.NonEmpty

  final case class Arr(value: ArraySeq[Yaml.NonEmpty], format: Specified[FormatPreference] = ___) extends Yaml.NonEmpty {
    def ++(that: Yaml.Arr): Yaml.Arr = Yaml.Arr(this.value ++ that.value, this.format.orElse(that.format))
  }

  final case class Obj(value: ArraySeq[(String, Yaml)], format: Specified[FormatPreference] = ___) extends Yaml.NonEmpty {
    def ++(that: Yaml.Obj): Yaml.Obj = Yaml.Obj(this.value ++ that.value, this.format.orElse(that.format))
  }

  def arr(value: Yaml.NonEmpty*): Yaml.Arr = Yaml.Arr(value.toArraySeq)

  def obj(value: (String, Yaml)*): Yaml.Obj = Yaml.Obj(value.toArraySeq)

  enum FormatPreference derives StrictEnum { case Inline, MultiLine }

  enum QuoteType derives StrictEnum { case Unquoted, SingleQuotes, DoubleQuotes }
  
  def fromString(string: String): Either[String, Yaml] =
    YamlParser.parse(string)

}
