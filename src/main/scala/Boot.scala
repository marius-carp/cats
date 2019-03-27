import catsLearn._
import cats._
import cats.data._
import cats.implicits._

object Boot extends App {


  val result = Conjunction(true) |+| Conjunction(false)

  println(result.unwrap)

}

