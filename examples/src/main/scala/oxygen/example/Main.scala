package oxygen.example

import oxygen.json.Json

object Main extends scala.App {

  //////////////////////////////////////////////////////////////////////////////////////////////////////
  //      Types
  //////////////////////////////////////////////////////////////////////////////////////////////////////

  // @oxygen.meta.K0.annotation.showDerivation[MyJsonDecoder]
  final case class Person(
      first: String,
      last: String,
      age: Option[Int],
  ) derives MyJsonDecoder

  @oxygen.meta.K0.annotation.showDerivation[MyJsonDecoder]
  enum Animal derives MyJsonDecoder {
    case Cat(name: String)
    case Dog(name: String, breed: String)
    case Squirrel
  }

  //////////////////////////////////////////////////////////////////////////////////////////////////////
  //      JsonDecoder
  //////////////////////////////////////////////////////////////////////////////////////////////////////

  extension (json: Json)
    def decodeTo[A: MyJsonDecoder as decoder]: Either[MyJsonDecoder.Error, A] =
      decoder.decodeJson(json)

  final case class Wrapped(person: Person) derives MyJsonDecoder

  val personJson1: Json = Json.parse(""" { "first": "F1", "last": "L1", "age": 100 } """).toOption.get
  val personJson2: Json = Json.parse(""" { "first": "F1", "last": "L1", "age": null } """).toOption.get
  val personJson3: Json = Json.parse(""" { "first": "F1", "last": "L1" } """).toOption.get
  val personJson3_1: Json = Json.parse(""" { "first": "F1", "age": null } """).toOption.get
  val personJson1_1: Json = Json.parse(""" { "first": true, "last": "L1", "age": 100 } """).toOption.get

  val personJson4: Json = Json.parse(""" { "person": { "first": "F1", "last": "L1", "age": 100 } } """).toOption.get
  val personJson5: Json = Json.parse(""" { "person": { "first": "F1", "last": "L1", "age": null } } """).toOption.get
  val personJson6: Json = Json.parse(""" { "person": { "first": "F1", "last": "L1" } } """).toOption.get

  println()
  println(s"=====| Person |=====")
  println()
  println(personJson1.decodeTo[Person])
  println(personJson1_1.decodeTo[Person])
  println(personJson2.decodeTo[Person])
  println(personJson3.decodeTo[Person])
  println(personJson3_1.decodeTo[Person])
  println()
  println(personJson4.decodeTo[Wrapped])
  println(personJson5.decodeTo[Wrapped])
  println(personJson6.decodeTo[Wrapped])
  println()

  val animalJson1: Json = Json.parse(""" { "Cat": { "name": "iam cat" } } """).toOption.get
  val animalJson1: Json = Json.parse(""" { "type: "Cat", "name": "iam cat" } """).toOption.get
  val animalJson2: Json = Json.parse(""" { "Dog": { "name": "iam doggo", "breed": "dog" } } """).toOption.get
  val animalJson3: Json = Json.parse(""" { "Squirrel": {} } """).toOption.get
  val animalJson4: Json = Json.parse(""" { "IDK": {} } """).toOption.get

  println()
  println(s"=====| Animal |=====")
  println()
  println(animalJson1.decodeTo[Animal])
  println(animalJson2.decodeTo[Animal])
  println(animalJson3.decodeTo[Animal])
  println(animalJson4.decodeTo[Animal])
  println()

}
