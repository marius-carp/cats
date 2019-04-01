package catsLearn.day5

import cats.Apply
import simulacrum.typeclass

@typeclass trait FlatMap[F[_]] extends Apply[F] {

  def flatMap[A, B](fa: F[A])(f: A => F[B]): F[B]

  def tailRec[A, B](a: A)(f: A => F[Either[A, B]]): F[B]

}

class FlatMapOps[F[_], A](fa: F[A])(implicit F: FlatMap[F]) {

  def flatMap[B](f: A => F[B]): F[B] = F.flatMap(fa)(f)

  def >>=[B](f: A => F[B]): F[B] = F.flatMap(fa)(f)

  def >>[B](fb: F[B]): F[B] = F.flatMap(fa)(_ => fb)

}
