package catsLearn.day3

import simulacrum.typeclass

/**
  * Created by frunza on 14.03.2019.
  */
@typeclass trait CanTruthy[A] { self =>
  def truthy(a: A): Boolean
}

object CanTruthy {
  def fromTruthy[A](f: A => Boolean): CanTruthy[A] = new CanTruthy[A] {
    override def truthy(a: A): Boolean = f(a)
  }
}


