package catsLearn.day7

import cats._
import cats.data._
import cats.implicits._
import cats.data.{ NonEmptyList => NEL }
import Validated.{ valid, invalid }

object ValidatedTest {


  val result = (
    valid[NEL[String], String]("event 1 ok") |@|
      invalid[NEL[String], String](NEL.of("envent 2 failed!")) |@|
      invalid[NEL[String], String](NEL.of("event 3 failerd!"))
  ) map (_ + _ + _)

  val errs: NEL[String] = result.fold(
    { l => l },
    { r => sys.error("invalid is expected")}
  )




}
