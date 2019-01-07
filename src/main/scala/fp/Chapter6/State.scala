package fp.Chapter6

case class State[S, +A](run: S => (A, S)) {

  def map[B](f: A => B): State[S, B] =
    State(x => run(x) match {
      case (a, s) =>
        (f(a), s)
    })

  def map2[B, C](s2: State[S, B])(f: (A, B) => C): State[S, C] = {
    State(x => run(x) match {
      case (a, s) =>
        run(s) match {
          case (b, ss) =>
            (f(a, b), ss)
        }
    })
  }

  def flatMap[B](f: A => State[S, B]): State[S, B] =
    State(x => run(x) match {
      case (a, s) =>
        f(a).run(s)
    })

  def get(): State[S, S] =
    State(x => (x, x))

  def set(s: S): State[S, Unit] =
    State(_ => ((), s))
}

object State {

  def unit[S, A](a: A): State[S, A] =
    State(x => (a, x))

  def sequence[S,A](sas: List[State[S, A]]): State[S, List[A]] =
    sas.foldRight(unit[S, List[A]](List()))((f, acc) => f.map2(acc)(_ :: _))

}


