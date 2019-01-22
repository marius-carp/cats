package fp.Chapter10

sealed trait WC

case class Stub(chars: String) extends WC
case class Part(lStub: String, words: Int, rStub: String) extends WC

object WC {

  def wcMonoid: Monoid[WC] = new Monoid[WC] {
    override def zero: WC = Stub("")

    override def op(a1: WC, a2: WC): WC = {
      (a1, a2) match {
        case (a: Stub, b: Stub) => Stub(a.chars + b.chars)
        case (a: Stub, b: Part) => Part(a.chars + b.lStub, b.words, b.rStub)
        case (a: Part, b: Stub) => Part(a.lStub, a.words, a.rStub + b.chars)
        case (a: Part, b: Part) => Part(a.lStub, a.words + (if ((a.rStub + b.lStub).isEmpty) 0 else 1) + b.words, b.rStub)
      }
    }
  }

  def countWords(s: String): Int = {
    val wc = Monoid.foldMapAsFold(s.toList, wcMonoid)(c =>
      if (c == ' ' || c == ','  || c == '.') Part("", 0, "")
      else Stub(c.toString))

    wc match {
      case Part (l, w, r) => w + (if (l.isEmpty) 0 else 1) + (if (r.isEmpty) 0 else 1)
      case Stub(c) => if (c.isEmpty) 0 else 1
    }

  }

}

