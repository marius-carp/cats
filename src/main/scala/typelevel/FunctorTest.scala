package typelevel

import cats.Functor
import cats.data.Nested
import cats.implicits._

object FunctorTest {

/*
  val intToJe: Option[Int] => Option[String] = Functor.functorOption.lift[Int, String](_.toString + " je")

  val result = intToJe(Some(21))*/

  val listOptions: List[Option[Int]] = List(Some(1), None, Some(2))
  val result: List[Option[Int]] = Functor[List].compose[Option].map(listOptions)(_ + 1)

  val nested: Nested[List, Option, Int] = Nested(listOptions)

  val result2 = nested.map(_ + 1)

}
