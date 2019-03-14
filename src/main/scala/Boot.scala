import catsLearn.{CanAppend, CanTruthy, TrafficLight}

import catsLearn.CanTruthy.ops._
import catsLearn.CanAppend.ops._
import simulacrum.op

object Boot extends App {

  implicit val intCanTruthy: CanTruthy[Int] = CanTruthy.fromTruthy({
    case 0 => false
    case 1 => true
  })

  implicit val intCanAppend: CanAppend[Int] = new CanAppend[Int] {
    override def append(a1: Int, a2: Int): Int = a1 + a2
  }

  print(1 |+| 2)

}

