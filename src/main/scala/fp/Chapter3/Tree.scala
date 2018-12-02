package fp.Chapter3

sealed trait Tree[+A]

case class Leaf[A](value: A) extends Tree[A]
case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]


object Tree {

  def size[A](tree: Tree[A]): Int =
    tree match {
      case Leaf(_) => 1
      case Branch(left, branch) => size(left) + size(branch)
    }

  def maximum(tree: Tree[Int]): Int =
    tree match {
      case Leaf(value) => value
      case Branch(left, right) => maximum(left).max(maximum(right))
    }

  def depth[A](tree: Tree[A]): Int = {
    def go(tree: Tree[A], depth: Int): Int = {
      tree match {
        case Leaf(_) => 1
        case Branch(Leaf(_), Leaf(_)) =>
          
      }

    }

    go(tree, 0, 0)

  }

}
