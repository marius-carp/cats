import catsLearn.{CanAppend, CanTruthy, TrafficLight}

import cats._
import cats.data._
import cats.implicits._

object Boot extends App {


  Apply[Option].ap({{(_: Int) + 3}.some})(3.some)

}

