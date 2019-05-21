package typelevel

import cats.Alternative
import cats.implicits._

import scala.collection.immutable

object AlternativeTest {

  val empty = Alternative[Vector].empty[Int]
  val pureOfFive = 5.pure[Vector]
  val concatenated = 7.pure[Vector] <+> 8.pure[Vector]
  val double: Int => Int = _ * 2
  val addFive: Int => Int = _ + 5
  val apForVectors = (double.pure[Vector] <+> addFive.pure[Vector]) ap concatenated


  trait Decoder[A] {
    def decode(in: String): Either[Throwable, A]
  }

  object Decoder {
    def from[A](f: String => Either[Throwable, A]): Decoder[A] =
      (in: String) => f(in)
  }

  implicit val decoderAlternative = new Alternative[Decoder] {
    override def pure[A](x: A): Decoder[A] = Decoder.from(Function.const(Right(x)))

    override def empty[A]: Decoder[A] = Decoder.from(Function.const(Left(new Error("No dice"))))

    override def combineK[A](x: Decoder[A], y: Decoder[A]): Decoder[A] =
      new Decoder[A] {
        override def decode(in: String): Either[Throwable, A] =
          x.decode(in).orElse(y.decode(in))
      }

    override def ap[A, B](ff: Decoder[A => B])(fa: Decoder[A]): Decoder[B] =
      new Decoder[B] {
        override def decode(in: String): Either[Throwable, B] =
          fa.decode(in).ap(ff.decode(in))
      }
  }

  def parseInt(s: String): Either[Throwable, Int] = Either.catchNonFatal(s.toInt)
  def parseIntFirstChar(s: String): Either[Throwable, Int] = Either.catchNonFatal(2 * Character.digit(s.charAt(0), 10))

  def decoder: Decoder[Int] = Decoder.from(parseInt) <+> Decoder.from(parseIntFirstChar)

  val result = decoder.decode("555")
  val result2 = decoder.decode("5a")

  def requestResource(a: Int): Either[(Int, String), (Int, Long)] = {
    if (a % 4 == 0) Left((a, "Bad request"))
    else if (a % 3 == 0) Left((a, "Server error"))
    else Right((a, 200L))
  }

  val partitionedResults: (Vector[(Int, String)], Vector[(Int, Long)]) = (requestResource _).pure[Vector].ap(Vector(5, 6, 7, 99, 1200, 8, 22)).separate

  def getRegionAndDistrict(pKey: Int): (Int, Vector[Int]) = (5 * pKey, (double.pure[Vector] <+> addFive.pure[Vector]).ap(pKey.pure[Vector]))

  val regionsWithDistricts = (getRegionAndDistrict _).pure[Vector].ap(Vector(5, 6, 7, 97, 1200, 8, 25))

  val regionIds = regionsWithDistricts.separate._1

  val districtIds = regionsWithDistricts.separate._2.flatten
}
