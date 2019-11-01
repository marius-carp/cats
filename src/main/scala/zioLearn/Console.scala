package zioLearn

sealed trait Console[+A]

case class Return[A](value: () => A) extends Console[A]
case class PrintLine[A](line: String, rest: Console[A]) extends Console[A]
case class ReadLine[A](rest: String => Console[A]) extends Console[A]

object Console extends App {

  def interpret[A](program: Console[A]): A = program match {
    case Return(value) =>
      value()
    case PrintLine(line, next) =>
      println(line)
      interpret(next)
    case ReadLine(next) =>
      interpret(next(scala.io.StdIn.readLine()))
  }

  def succeed[A](a: => A): Console[A] = Return(() => a)
  def printLine(line: String): Console[Unit] = PrintLine(line, succeed())
  def readLine: Console[String] = ReadLine(line => succeed(line))

  implicit class ConsoleSyntax[+A](self: Console[A]) {
    def map[B](f: A => B): Console[B] =
      self.flatMap(a => succeed(f(a)))

    def flatMap[B](f: A => Console[B]): Console[B] = {
      self match {
        case Return(a) => f(a())
        case PrintLine(line, next) =>
          PrintLine(line, next.flatMap(f))
        case ReadLine(next) =>
          ReadLine(line => next(line).flatMap(f))
      }
    }
  }

  val example: Console[String] = {
    for {
      _ <- printLine("What's your name?")
      name <- readLine
      _ <- printLine(name)
    } yield name
  }

  interpret(example)

}
