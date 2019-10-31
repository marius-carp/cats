package workshop.json

import workshop.example.Person

trait JsonWriter[A] {

  def write(value: A): Json

}


object JsonWriterInstances {

  implicit val stringWriter = new JsonWriter[String] {
    override def write(value: String): Json =
      JsString(value)
  }

  implicit val personWriter = new JsonWriter[Person] {
    override def write(value: Person): Json =
      JsObject(
        Map(
          "name" -> JsString(value.name),
          "emai" -> JsString(value.email)
        )
      )
  }

//  val optionIntWriter: JsonWriter[Option[Int]] = ???
//  val optionPersonWriter: JsonWriter[Option[Person]] = ???


  implicit def optionWriter[A](implicit writer: JsonWriter[A]): JsonWriter[Option[A]] =
    new JsonWriter[Option[A]] {
      override def write(option: Option[A]): Json = option match {
        case Some(aValue) => writer.write(aValue)
        case None => JsNull
      }
    }
}

object JsonSyntax {
  implicit class JsonWriterOps[A](value: A) {
    def toJson(implicit w: JsonWriter[A]): Json =
      w.write(value)
  }
}