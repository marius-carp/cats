import cats.data._
import cats.data.Validated._
import cats.implicits._
import workshop.example.Person
import workshop.json.Json
import workshop.json.JsonWriterInstances._
import workshop.json.JsonSyntax._

object Boot extends App {

  /*val str = "Frunza"
  val person = Person("frunza", "frz@gg.com")
  val optionPerson: Option[Person] = Some(person)

  val jsonStr = Json.toJson(str)
  val jsonPerson = Json.toJson(person)
  val jsonOptionPerson = Json.toJson(optionPerson)


  println(jsonStr)
  println(jsonPerson)
  println(jsonOptionPerson)
  */

  private def validateDouble(str: String): ValidatedNec[String, Double] = ???





}
