package catsLearn.day9

import cats._
import cats.data._
import cats.implicits._

object KleisliTest {

  val f = Kleisli { (x: Int) => (x + 1).some }
  val g = Kleisli { (x: Int) => (x * 100).some }

  val result = 4.some >>= (f compose g).run
  val result2 = 4.some >>= (f andThen g).run

  val l = f.lift[List]

  val result3 = List(1, 2, 3, 4) >>= l.run

}
