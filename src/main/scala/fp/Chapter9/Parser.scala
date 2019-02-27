package fp.Chapter9

import scala.util.matching.Regex
import scala.language.implicitConversions
import scala.language.higherKinds

trait Parser[A] {

}

case class ParserError()

trait Parsers[ParserError, Parser[+_]] { self =>
  implicit def string(s: String): Parser[String]
  implicit def operators[A](p: Parser[A]): ParserOps[A] = ParserOps[A](p)
  implicit def asStringParser[A](a: A)(implicit f: A => Parser[String]): ParserOps[String] = ParserOps(f(a))
  implicit def regex(r: Regex): Parser[String]


  def run[A](p: Parser[A])(input: String): Either[ParserError, A]
  def char(c: Char): Parser[Char] = string(c.toString) map (_.charAt(0))
  def orString(s1: String, s2: String): Parser[String]
  def or[A](s1: Parser[A], s2: Parser[A]): Parser[A]
  def listOfN[A](n: Int, p: Parser[A]): Parser[List[A]]
  def many[A](p: Parser[A]): Parser[List[A]]
  def succeed[A](a: A): Parser[A] = string("") map(_ => a)
  def slice[A](p: Parser[A]): Parser[String]
  def map2[A, B, C](p: Parser[A], p2: Parser[B])(f: (A, B) => C): Parser[C] = {
    (p ** p2).map { case (a, b) => f(a, b) }
  }
  def many1[A](p: Parser[A]): Parser[List[A]] = {
    map2(p, many(p))((a, as) => as :+ a)
  }
  def manyAlt[A](p: Parser[A]): Parser[List[A]] =
    map2(p, asLazy(manyAlt(p)))((a, acc) => acc :+ a) or succeed(Nil)

  def asLazy[A](p: => Parser[A]): Parser[A] = p

  def flatMap[A, B](p: Parser[A])(f: A => Parser[B]): Parser[B]

  case class ParserOps[A](p: Parser[A]) {
    def |[B >: A](p2: Parser[B]): Parser[B] = self.or(p, p2)
    def or[B >: A](p2: => Parser[B]): Parser[B] = self.or(p, p2)
    def many(p: Parser[A]): Parser[List[A]] = ???
    def map[B](f: A => B): Parser[B] = ???
    def product[B](p: Parser[A], p2: Parser[B]): Parser[(A, B)] = ???
    def **[B](p2: Parser[B]): Parser[(A, B)] = product(p, p2)
  }

}
