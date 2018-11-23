import cats.{Applicative, Functor, Monad}
import cats.Functor.ops._

object Boot extends App {

  val ops = Monad[Option]
  val lst = Monad[List]

  val resultOps = ops.flatMap(Option(1))(x => if (x > 0) Some(x) else None)
  val resultLst = lst.flatMap(List(1, 2, 3))(x => List.fill(x)(x))


  println(resultOps)

}