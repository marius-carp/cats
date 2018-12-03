import fp.Chapter3._

object Boot extends App {


  val branch121 = Branch(Leaf(3), Leaf(4))
  val branch1222 = Branch(Leaf(6), Leaf(7))

  val branch122 = Branch(Leaf(12), branch1222)

  val branch11 = Branch(Leaf(1), Leaf(2))
  val branch12 = Branch(branch121, branch122)

  val tree = Branch(branch11, branch12)


  println(Tree.depth(tree))

}

