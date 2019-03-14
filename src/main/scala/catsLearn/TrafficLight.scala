package catsLearn

import cats._
import cats.data._
import cats.implicits._

sealed trait TrafficLight

object TrafficLight {
  def red: TrafficLight = Red
  def yellow: TrafficLight = Yellow
  def green: TrafficLight = Green

  case object Red extends TrafficLight
  case object Yellow extends TrafficLight
  case object Green extends TrafficLight

  implicit val trafficLightEq: Eq[TrafficLight] = new Eq[TrafficLight] {
    override def eqv(x: TrafficLight, y: TrafficLight): Boolean =
      x == y
  }


}


