package fp.Chapter7


import java.util.concurrent.{TimeUnit, Callable, Future, ExecutorService}


case class SimpleFuture[A](a: A) extends Future[A] {
  def get: A = a
  def get(timeout: Long, unit: TimeUnit): A = get
  def cancel(evenIfRunning: Boolean): Boolean = false
  def isDone: Boolean = true
  def isCancelled: Boolean = false
}

object Par {

  type Par[A] = ExecutorService => Future[A]

  def unit[A](a: A): Par[A] = _ => SimpleFuture(a)

  def map2_old[A, B, C](a: Par[A], b: Par[B])(f: (A, B) => C): Par[C] = es => {
    val af = run(es)(a)
    val bf = run(es)(b)

    SimpleFuture(f(af.get(), bf.get()))
  }

  def fork[A](a: => Par[A]): Par[A] = es => es.submit(new Callable[A] {
    override def call(): A = a(es).get()
  })

  def async[A](a: => A): Par[A] = fork(unit(a))

  def run[A](s: ExecutorService)(a: Par[A]): Future[A] = a(s)

  def asyncF[A, B](f: A => B): A => Par[B] = a => async(f(a))

  def sortPar(l: Par[List[Int]]): Par[List[Int]] =
    map_old(l)(_.sorted)

  def map_old[A, B](a: Par[A])(f: A => B): Par[B] =
    map2_old(a, unit(()))((a, _) => f(a))

  def map[A, B](fa: Par[A])(f: A => B): Par[B] = es => {
    SimpleFuture(f(Par.run(es)(fa).get))
  }

  def product[A,B](fa: Par[A], fb: Par[B]): Par[(A,B)] = es => {
    val a = Par.run(es)(fa)
    val b = Par.run(es)(fb)

    SimpleFuture((a.get, b.get))
  }

  def map2[A, B, C](a: Par[A], b: Par[B])(f: (A, B) => C): Par[C] = {
    map(product(a,b)){ case (x,y) => f(x,y) }
  }

  def parMap[A, B](l: List[A])(f: A => B): Par[List[B]] = {
    es => {
      es.submit(() => l.map(f))
    }
  }

  def parMap_map[A, B](l: List[A])(f: A => B): Par[List[B]] = {
    val fbs: List[Par[B]] = l.map(asyncF(f))

    sequence(fbs)
  }

  def sequence[A](l: List[Par[A]]): Par[List[A]] = {
    es => {
      es.submit(() => l.map(r => r(es).get()))
    }
  }

  def parFilter[A](l: List[A])(f: A => Boolean): Par[List[A]] = {
    es => {
      es.submit(() => l.filter(f))
    }
  }

  def choice[A](a: Par[Boolean])(ifTrue: Par[A], ifFalse: Par[A]): Par[A] = {
    es => {
      val result = if(a(es).get())
        ifTrue(es).get()
      else
        ifFalse(es).get()

      SimpleFuture(result)
    }
  }

  def choiceN[A](a: Par[Int])(choices: List[Par[A]]): Par[A] = {
    es => {
      val aa = a(es).get()
      choices.lift(aa) match {
        case Some(value) => value(es)
        case _ => throw new IllegalArgumentException("Element doesn't exist")
      }
    }
  }

  def choiceAsChoiceN[A](a: Par[Boolean])(ifTrue: Par[A], ifFalse: Par[A]): Par[A] = {
    choiceN(map(a)(r => if(r) 0 else 1))(List(ifTrue, ifFalse))
  }

  def choiceMap[A, B](a: Par[A])(choices: Map[A, Par[B]]): Par[B] = {
    es => {
      val aa = a(es).get()
      val b = choices.getOrElse(aa, throw new IllegalArgumentException("Element doesn't exist"))

      b(es)
    }
  }

  def chooser[A, B](a: Par[A])(choices: A => Par[B]): Par[B] = {
    es => {
      val aa = a(es).get
      choices(aa)(es)
    }
  }

  def choiceAsChooser[A](a: Par[Boolean])(ifTrue: Par[A], ifFalse: Par[A]): Par[A] = {
    chooser(a){ r => if(r) ifTrue else ifFalse }
  }

  def choiceNAsChooser[A](a: Par[Int])(choices: List[Par[A]]): Par[A] = {
    chooser(a){ r =>
      choices.lift(r) match {
        case Some(value) => value
        case _ => throw new IllegalArgumentException("Element doesn't exist")
      }
    }
  }

  def flatMap[A, B](a: Par[A])(f: A => Par[B]): Par[B] = {
    es => {
      val aa = a(es).get
      f(aa)(es)
    }
  }

  def join[A](a: Par[Par[A]]): Par[A] = {
    es => {
      SimpleFuture(a(es).get()(es).get())
    }
  }
}
