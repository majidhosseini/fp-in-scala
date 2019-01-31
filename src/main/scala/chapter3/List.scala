package me.fpinscala.chapter3

sealed trait Li[+A]
case object N extends Li[Nothing]
case class C[+A](head: A, tail: Li[A]) extends Li[A]


object Li {
  def apply[A](as: A*): Li[A] =
    if (as.isEmpty) N
    else C(as.head, apply(as.tail: _*))
}