package oxygen.json

import oxygen.predef.core.*

object SecretUtil {

  def splitArrayElems(elems: ArraySeq[Ior[PlainTextJson, SecretJson]]): Ior[PlainTextJsonArray, SecretJsonArray] = {
    val plainTextOptElems: ArraySeq[Option[PlainTextJson]] = elems.map(_.leftOption)
    val secretOptElems: ArraySeq[Option[SecretJson]] = elems.map(_.rightOption)
    def plainTextArray: PlainTextJsonArray = PlainTextJsonArray(plainTextOptElems.map(_.getOrElse(PlainTextJson.Null)))
    def secretArray: SecretJsonArray = SecretJsonArray(secretOptElems.map(_.getOrElse(SecretJson.Null)))

    (plainTextOptElems.exists(_.nonEmpty), secretOptElems.exists(_.nonEmpty)) match
      case (_, false)    => Ior.Left(plainTextArray)
      case (true, true)  => Ior.Both(plainTextArray, secretArray)
      case (false, true) => Ior.Right(secretArray)
  }

  def splitObjectElems(fields: ArraySeq[(String, Ior[PlainTextJson, SecretJson])]): Ior[PlainTextJsonObject, SecretJsonObject] = {
    val plainTextPairs: ArraySeq[(String, PlainTextJson)] = fields.flatMap { case (k, v) => v.leftOption.map((k, _)) }
    val secretPairs: ArraySeq[(String, SecretJson)] = fields.flatMap { case (k, v) => v.rightOption.map((k, _)) }

    val plainTextJsonObject: PlainTextJsonObject = PlainTextJsonObject(plainTextPairs)
    val secretJsonObject: SecretJsonObject = SecretJsonObject(secretPairs)

    (plainTextPairs.nonEmpty, secretPairs.nonEmpty) match
      case (_, false)    => Ior.Left(plainTextJsonObject)
      case (true, true)  => Ior.Both(plainTextJsonObject, secretJsonObject)
      case (false, true) => Ior.Right(secretJsonObject)
  }

  extension [A, B](self: Ior[A, B]) {

    def leftOption: Option[A] = self match
      case Ior.Both(left, _) => left.some
      case Ior.Left(left)    => left.some
      case Ior.Right(_)      => None

    def rightOption: Option[B] = self match
      case Ior.Both(_, right) => right.some
      case Ior.Right(right)   => right.some
      case Ior.Left(_)        => None

  }

}
