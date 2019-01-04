package fp.Chapter6

import fp.Chapter6.SimpleRNG.Rand

import scala.annotation.tailrec

trait RNG {

  def nextInt: (Int, RNG)

}

object RNG {
  def Simple(seed: Long): SimpleRNG = SimpleRNG(seed)
}

case class SimpleRNG(seed: Long) extends RNG {
  override def nextInt: (Int, RNG) = {
    val newSeed = (seed * 0x5DEECE66DL + 0xBL) & 0xFFFFFFFFFFFFL
    val nextRNG = SimpleRNG(newSeed)
    val n = (newSeed >>> 16).toInt

    (n, nextRNG)
  }

  val int: Rand[Int] = _.nextInt
}

object SimpleRNG {
  def randomPair(rng: RNG): ((Int, Int), RNG) = {
    val (int1, rng1) = rng.nextInt
    val (int2, rng2) = rng1.nextInt

    ((int1, int2), rng2)
  }

  def positiveInt(rng: RNG): (Int, RNG) = {
    val (nextInt, nextRNG) = rng.nextInt

    if(nextInt == Integer.MIN_VALUE)
      positiveInt(nextRNG)
    else
      (nextInt, nextRNG)
  }

  def double(rng: RNG): (Double, RNG) = {
    val (i, rngDouble) = positiveInt(rng)

    (i.toDouble / Integer.MAX_VALUE, rngDouble)
  }

  def intDouble(rng: RNG): ((Int, Double), RNG) = {
    val (i, rngInt) = rng.nextInt
    val (d, rngDouble) = double(rngInt)

    ((i, d), rngDouble)
  }

  def doubleInt(rng: RNG): ((Double, Int), RNG) = {
    val (d, rngDouble) = double(rng)
    val (i, rngInt) = rngDouble.nextInt

    ((d, i), rngInt)
  }

  def double3(rng: RNG): ((Double, Double, Double), RNG) = {
    val (d1, rng1) = double(rng)
    val (d2, rng2) = double(rng1)
    val (d3, rng3) = double(rng2)

    ((d1, d2, d3), rng3)
  }

  def ints(count: Int)(rng: RNG): (List[Int], RNG) = {
    @tailrec
    def loop(count: Int, rng: RNG, acc: List[Int]): (List[Int], RNG) = {
      if(count == 0)
        (acc, rng)
      else {
        val (i, nextRng) = rng.nextInt

        loop(count - 1, nextRng, acc :+ i)
      }
    }

    loop(count, rng, List.empty[Int])
  }

  type Rand[+A] = RNG => (A, RNG)

  def unit[A](a: A): Rand[A] =
    rng => (a, rng)

  def map[A, B](s: Rand[A])(f: A => B): Rand[B] =
    rng => {
      val (a, rng2) = s(rng)
      (f(a), rng2)
    }

  def positiveMax(n: Int): Rand[Int] = {
    map(positiveInt)(i => i / (Integer.MAX_VALUE / n))
  }

  def mapDouble: Rand[Double] = {
    map(positiveInt)(i => i.toDouble / Integer.MAX_VALUE)
  }

  def map2[A, B, C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] = {
    rng => {
      val (a, rng2) = ra(rng)
      val (b, rng3) = rb(rng2)

      (f(a, b), rng3)
    }
  }

  def mapIntDouble: Rand[(Int, Double)] =
    map2(_.nextInt, mapDouble) {
      case (a, b) => (a, b)
    }

  def mapDoubleInt: Rand[(Double, Int)] =
    map2(mapDouble, _.nextInt) {
      case (a, b) => (a, b)
    }

  def sequence[A](fs: List[Rand[A]]): Rand[List[A]] =
    fs.foldRight(unit(List[A]()))( (f, acc) => map2(f, acc)((a, b) => a :: b))

  def intsSequence(n: Int): Rand[List[Int]] =
    sequence(List.fill(n)(rng => rng.nextInt))
}
