package catsLearn.day7

import cats._
import cats.data._
import cats.implicits._

import cats.data.{NonEmptyList=>NEL}

object IorTest {

  val result = for {
    e1 <- Ior.right[NEL[String], Int](1)
    e2 <- Ior.both[NEL[String], Int](NEL.of("event 2 warning"), e1 + 1)
    e3 <- Ior.both[NEL[String], Int](NEL.of("event 3 warning"), e2 + 1)
  } yield e1 |+| e2  |+| e3

}
