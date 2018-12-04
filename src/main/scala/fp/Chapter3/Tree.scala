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
    tree match {
      case Leaf(_) => 0
      case Branch(left, right) =>
        1 + depth(left).max(depth(right))
    }
  }

  def map[A, B](tree: Tree[A])(f: A => B): Tree[B] = {
    tree match {
      case Leaf(value) => Leaf(f(value))
      case Branch(left, right) =>
        Branch(map(left)(f), map(right)(f))
    }
  }

  def fold[A, B](tree: Tree[A], f: A => B)(combine: (B, B) => B): B =
    tree match {
      case Leaf(_) => f(tree)
      case Branch(left, right) =>
        combine(
          combine(left, right)(combine),
          combine(right, left)(combine)
        )
    }

  def sizeFold[A](tree: Tree[A]): Int =
    fold(tree, (_: A) => 1)((left, right) => 1 + left + right)

  def maximumFold(tree: Tree[Int]): Int =
    fold(tree, (x: Int) => x)((left, right) => right.max(left))

  def depthFold[A](tree: Tree[A]): Int =
    fold(tree, (_: A) => 1)((left, right) => 1 + left.max(right))

  def mapFold[A, B](tree: Tree[A])(f: A => B): Tree[B] =
    fold(tree, (x: A) => Leaf(f(x)): Tree[B])((left, right) => Branch(left, right))

}
