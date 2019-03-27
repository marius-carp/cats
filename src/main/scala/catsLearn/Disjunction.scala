package catsLearn

import cats._
import cats.data._
import cats.implicits._

class Disjunction(val unwrap: Boolean) extends AnyVal

object Disjunction {

  @inline
  def apply(unwrap: Boolean): Disjunction = new Disjunction(unwrap)

  implicit val disjunctionMonoid: Monoid[Disjunction] = new Monoid[Disjunction] {
    override def empty: Disjunction = Disjunction(false)

    override def combine(x: Disjunction, y: Disjunction): Disjunction = {
      Disjunction(x.unwrap || y.unwrap)
    }
  }

  implicit val disjunctionEq: Eq[Disjunction] =
    (x: Disjunction, y: Disjunction) => x.unwrap == y.unwrap

}

class Conjunction(val unwrap: Boolean) extends AnyVal

object Conjunction {

  @inline
  def apply(unwrap: Boolean): Conjunction = new Conjunction(unwrap)

  implicit val conjunctionMonoid: Monoid[Conjunction] = new Monoid[Conjunction] {
    override def empty: Conjunction = Conjunction(false)

    override def combine(x: Conjunction, y: Conjunction): Conjunction = {
      Conjunction(x.unwrap && y.unwrap)
    }
  }

  implicit val conjunctionEq: Eq[Conjunction] =
    (x: Conjunction, y: Conjunction) => x.unwrap == y.unwrap

}


