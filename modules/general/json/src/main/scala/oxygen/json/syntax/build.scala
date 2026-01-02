package oxygen.json.syntax

import oxygen.json.{Json, JsonEncoder}
import oxygen.json.syntax.json.*
import oxygen.predef.core.*

object build {

  extension (self: Json.Obj) {

    def addField(key: String)(value: Json): Json.Obj =
      if self.value.exists(_._1 == key) then
        Json.Obj(
          self.value.map {
            case (`key`, v) => (key, v.merge(value))
            case tup        => tup
          },
        )
      else
        Json.Obj(self.value :+ (key, value))

    def addFieldOptional(key: String)(value: Option[Json]): Json.Obj = value match
      case Some(value) => addField(key)(value)
      case None        => self

    def addFields(pairs: (String, Json)*): Json.Obj =
      pairs.foldLeft(self) { case (acc, (key, value)) => acc.addField(key)(value) }

    def addFieldsOptional(pairs: (String, Option[Json])*): Json.Obj =
      addFields(pairs.collect { case (key, Some(value)) => (key, value) }*)

    def addFieldCustom[A](key: String, value: A, filter: A => Boolean = (_: A) => true)(f: A => Json): Json.Obj =
      if filter(value) then addField(key)(f(value))
      else self

    def addFieldCustomSeq[A](key: String, value: Seq[A], filter: Seq[A] => Boolean = (_: Seq[A]) => true)(f: A => Json): Json.Obj =
      if filter(value) then addField(key)(Json.arr(value.map(f)*))
      else self

    def addFieldEncoded[A: JsonEncoder](key: String, value: A, filter: A => Boolean = (_: A) => true): Json.Obj =
      addFieldCustom[A](key, value, filter)(_.toJsonAST)

    def addOptionalFieldCustom[A](key: String, value: Option[A], filter: A => Boolean = (_: A) => true)(f: A => Json): Json.Obj =
      addFieldOptional(key)(value.collect { case v if filter(v) => f(v) })

    def addOptionalFieldEncoded[A: JsonEncoder](key: String, value: Option[A], filter: A => Boolean = (_: A) => true): Json.Obj =
      addOptionalFieldCustom[A](key, value, filter)(_.toJsonAST)

    def addSpecifiedFieldCustom[A](key: String, value: Specified[A], filter: A => Boolean = (_: A) => true)(f: A => Json): Json.Obj =
      addOptionalFieldCustom[A](key, value.toOption, filter)(f)

    def addSpecifiedFieldEncoded[A: JsonEncoder](key: String, value: Specified[A], filter: A => Boolean = (_: A) => true): Json.Obj =
      addSpecifiedFieldCustom[A](key, value, filter)(_.toJsonAST)

  }

}
