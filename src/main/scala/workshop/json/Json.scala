package workshop.json

import workshop.example.Person

sealed trait Json

final case class JsObject(get: Map[String, Json]) extends Json
final case class JsString(get: String) extends Json
final case class JsNumber(get: Double) extends Json
final case class JsBoolean(get: Boolean) extends Json
case object JsNull extends Json

object Json {

  def toJson[A](value: A)(implicit w: JsonWriter[A]): Json = {
    w.write(value)
  }

  /*val str = "Frunza"
  val person = Person("frunza", "frz@gg.com")
  val optionPerson: Option[Person] = Some(person)

  val jsonStr = Json.toJson(str)
  val jsonPerson = Json.toJson(person)
  val jsonOptionPerson = Json.toJson(optionPerson)


  println(jsonStr)
  println(jsonPerson)
  println(jsonOptionPerson)*/

}