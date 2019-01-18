package fp.Chapter9

trait Parser[A] {

}

case class ParserError()

trait Parsers[ParserError, Parser[_]] { self =>
  implicit def string[s: String]: Parser[String]
  implicit def operators[A](p: Parser[A]): ParserOps[A] = ParserOps[A](p)
  implicit def asStringParser[A](a: A)(implicit f: A => Parser[String]): ParserOps[String] = ParserOps(f(a))


  def run[A](p: Parser[A])(input: String): Either[ParserError, A]
  def char(c: Char): Parser[Char] = string(c.toString) map (_.charAt(0))
  def orString(s1: String, s2: String): Parser[String]
  def or[A](s1: Parser[A], s2: Parser[A]): Parser[A]
  def listOfN[A](n: Int, p: Parser[A]): Parser[List[A]]
  def succeed[A](a: A): Parser[A] = string("") map(_ => a)

  case class ParserOps[A](p: Parser[A]) {
    def |[B >: A](p2: Parser[B]): Parser[B] = self.or(p, p2)
    def or[B >: A](p2: => Parser[B]): Parser[B] = self.or(p, p2)
    def many(p: Parser[A]): Parser[List[A]] = ???
    def map[B](f: A => B): Parser[B] = ???
  }

}
