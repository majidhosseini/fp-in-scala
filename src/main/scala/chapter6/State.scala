package me.fpinscala.chapter6

import State._

case class State[S, +A](run: S => (A,S)) {
  def map[B](f: A => B): State[S, B] = flatMap { a =>
    unit(f(a))
  }

  def map2[B, C](sb: State[S, B])(f: (A, B) => C): State[S, C] = flatMap { a =>
    sb.map { b =>
      f(a, b)
    }
  }

  def flatMap[B](f: A => State[S, B]): State[S, B] = State(s => {
    val (a, s1) = run(s)
    f(a).run(s1)
  })
}

object State {
  //type State[S, +A] = S => (A, S)
  type Rand[A] = State[RNG, A]

  def unit[S, A](a: A): State[S, A] = State(s => (a, s))

  def sequence[S, A](fs: List[State[S, A]]): State[S, List[A]] = fs.foldRight(unit[S, List[A]](List())) {
    (s, l) => s.map2(l)(_ :: _)
  }

  def modify[S](f: S => S): State[S, Unit] = for {
    s <- get
    _ <- set(f(s))
  } yield ()

  def get[S]: State[S, S] = State(s => (s, s))
  def set[S](s: S): State[S, Unit] = State(_ => ((), s))
}
