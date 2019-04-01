package catsLearn.day3

import simulacrum.{op, typeclass}

@typeclass trait CanAppend[A] {
  @op("|+|") def append(a1: A, a2: A): A
}


