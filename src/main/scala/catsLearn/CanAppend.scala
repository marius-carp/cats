package catsLearn

import cats._
import cats.data._
import cats.implicits._
import simulacrum.{op, typeclass}

@typeclass trait CanAppend[A] {
  @op("|+|") def append(a1: A, a2: A): A
}


