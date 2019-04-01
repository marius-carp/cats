import catsLearn._
import cats._
import cats.data._
import cats.implicits._
import catsLearn.day5.KnightPos

object Boot extends App {


  val result = KnightPos(6, 2) canReachIn3 KnightPos(6, 1)

  println(result)


}

