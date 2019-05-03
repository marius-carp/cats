package typelevel

import cats.Semigroup
import cats.implicits._
import cats.InvariantMonoidal

object InvariantMonoidalTest {

  def unit: Semigroup[Unit] = new Semigroup[Unit] {
    override def combine(x: Unit, y: Unit): Unit = ()
  }

  def product[A, B](fa: Semigroup[A], fb: Semigroup[B]): Semigroup[(A, B)] = new Semigroup[(A, B)] {
    override def combine(x: (A, B), y: (A, B)): (A, B) =
      (x, y) match {
        case ((xa, xb), (ya, yb)) => fa.combine(xa, ya) -> fb.combine(xb, yb)
      }
  }

  case class Foo(a: String, c: List[Double])

  implicit val fooSemigroup: Semigroup[Foo] =
    (implicitly[Semigroup[String]], implicitly[Semigroup[List[Double]]]).imapN(Foo.apply)(Function.unlift(Foo.unapply))

  val result = Foo("Hello", List(0.0)) |+| Foo("World", Nil) |+| Foo("!", List(1.1, 2.2))

  type CSV = List[String]

  trait CsvCodec[A] {
    def read(s: CSV): (Option[A], CSV)
    def write(a: A): CSV

    def forAll: Unit = {
      (c: CsvCodec[A], a: A) => c.read(c.write(a)) == ((Some(a), List()))
    }
  }

  trait CCUnit {
    def unit: CsvCodec[Unit] = new CsvCodec[Unit] {
      override def read(s: CSV): (Option[Unit], CSV) = (Some(()), s)

      override def write(a: Unit): CSV = List.empty
    }
  }

  trait CCProduct {
    def product[A, B](fa: CsvCodec[A], fb: CsvCodec[B]): CsvCodec[(A, B)] =
      new CsvCodec[(A, B)] {
        override def read(s: CSV): (Option[(A, B)], CSV) = {
          val (a1, s1) = fa.read(s)
          val (a2, s2) = fb.read(s1)

          ((a1, a2).mapN(_ -> _), s2)
        }

        override def write(a: (A, B)): CSV =
          fa.write(a._1) ++ fb.write(a._2)
      }
  }

  trait CCImap {
    def imap[A, B](fa: CsvCodec[A])(f: A => B)(g: B => A): CsvCodec[B] =
      new CsvCodec[B] {
        override def read(s: CSV): (Option[B], CSV) = {
          val (a1, s1) = fa.read(s)
          (a1.map(f), s1)
        }

        override def write(a: B): CSV =
          fa.write(g(a))
      }
  }

  implicit val csvCodecIsInvariantMonoid: InvariantMonoidal[CsvCodec] =
    new InvariantMonoidal[CsvCodec] with CCUnit with CCProduct with CCImap

  val stringCodec: CsvCodec[String] =
    new CsvCodec[String] {
      override def read(s: CSV): (Option[String], CSV) = (s.headOption, s.drop(1))

      override def write(a: String): CSV = List(a)
    }

  def numericSystemCodec(base: Int): CsvCodec[Int] = new CsvCodec[Int] {
    override def read(s: CSV): (Option[Int], CSV) =
      (s.headOption.flatMap(head => scala.util.Try(Integer.parseInt(head, base)).toOption), s.drop(1))

    override def write(a: Int): CSV =
      List(Integer.toString(a, base))
  }

  case class BinDec(binary: Int, decimal: Int)

  val binDecCodec: CsvCodec[BinDec] =
    (numericSystemCodec(2), numericSystemCodec(10))
        .imapN(BinDec.apply)(Function.unlift(BinDec.unapply))

  case class Foo2(name: String, bd1: BinDec, bd2: BinDec)

  val fooCodec: CsvCodec[Foo2] = (stringCodec, binDecCodec, binDecCodec)
    .imapN(Foo2.apply)(Function.unlift(Foo2.unapply))

  val foo = Foo2("foo", BinDec(10, 10), BinDec(20, 20))

  val result2 = fooCodec.write(foo)

  val result3 = fooCodec.read(result2)

  val result4 = fooCodec.read(fooCodec.write(foo)) == ((Some(foo), List()))

}
