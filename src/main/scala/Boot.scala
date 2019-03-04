import fp.Chapter14.{IO, Player}

object Boot extends App {

  def ReadLine: IO[String] =
    IO {
      scala.io.StdIn.readLine()
    }

  def PrintLine(msg: String): IO[Unit] =
    IO {
      println(msg)
    }

  def fahrenheitToCelsius(f: Double): Double =
    (f - 32) * 5.0/9.0

  def converter: IO[Unit] = for {
    _ <- PrintLine("enter a temperature in degrees f: ")
    d <- ReadLine.map(_.toDouble)
    _ <- PrintLine(fahrenheitToCelsius(d).toString)
  } yield ()

  converter
}

