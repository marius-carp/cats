package catsLearn.day8

import cats._
import cats.data._
import cats.free.Free
import cats.implicits._
import catsLearn.day8.CharToy._


trait FixE[F[_], E]

object FixE {
  case class Fix[F[_], E](f: F[FixE[F, E]]) extends FixE[F, E]
  case class Throwy[F[_], E](e: E) extends FixE[F, E]

  def fix[E](toy: CharToy[FixE[CharToy, E]]): FixE[CharToy, E] =
    Fix[CharToy, E](toy)

  def throwy[F[_], E](e: E): FixE[F, E] = Throwy(e)

  def catchy[F[_]: Functor, E1, E2](ex: => FixE[F, E1])(f: E1 => FixE[F, E2]): FixE[F, E2] =
    ex match {
      case Fix(x) => Fix[F, E2](Functor[F].map(x){ r => catchy(r)(f)})
      case Throwy(e) => f(e)
    }


  case class IncompleteException()
  def subroutine = fix[IncompleteException](
    output('A', throwy[CharToy, IncompleteException](IncompleteException())))

  def program = catchy[CharToy, IncompleteException, Nothing](subroutine) { _ =>
    fix[Nothing](bell(fix[Nothing](done)))
  }

  val subroutineFree = outputFree('A')

  val programFree = for {
    _ <- subroutineFree
    _ <- bellFree
    _ <- doneFree
  } yield()

  def showProgram[R: Show](p: Free[CharToy, R]): String =
    p.fold({ r: R => "return " + Show[R].show(r) + "\n" },
      {
        case CharOutput(a, next) =>
          "output " + Show[Char].show(a) + "\n" + showProgram(next)
        case CharBell(next) =>
          "bell " + "\n" + showProgram(next)
        case CharDone() =>
          "done\n"
      }
    )

  val result = showProgram(programFree)
  val result2 = showProgram(pure('A') flatMap outputFree)
  val result3 = showProgram(outputFree('A') flatMap pure)
  val result4 = showProgram(outputFree('A') flatMap { _ => (doneFree flatMap { _ => outputFree('C') }) })
}
