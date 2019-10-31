package validated

trait CellResult[+A] {

}

case class CellResultSuccess[A](a: A) extends CellResult[A]
case object CellResultError extends CellResult[Nothing]
