import fp.Chapter4._
import fp.Chapter3._

object Boot extends App {


  val s : Either[String, Int] = Right(21).asInstanceOf[Either[String, Int]]

  println(s.map(_ + 1))

}

