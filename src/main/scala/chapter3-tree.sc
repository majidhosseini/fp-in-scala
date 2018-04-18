import scala.math.max

sealed trait Tree[+A]
case class Leaf[A](value: A) extends Tree[A]
case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]

// 3.25
def size[A](t: Tree[A]): Int = t match {
  case Leaf(_) => 1
  case Branch(left, right) => (size(left) + size(right))
}

// 3.26
def maximum[A](t: Tree[A])(f: (A,A) => A): A = t match {
  case Leaf(x) => x
  case Branch(left, right) => f(maximum(left)(f), maximum(right)(f))
}

// 3.27
def depth[A](t: Tree[A]): Int = t match {
  case Leaf(_) => 1
  case Branch(left, right) => depth(left) max depth(right)
}

// 3.28
def map[A,B](t: Tree[A])(f: A => B): Tree[B] = t match {
  case Leaf(x) => Leaf(f(x))
  case Branch(left, right) => Branch(map(left)(f), map(right)(f))
}

// 3.29
def fold[A](t: Tree[A], z: A)(f: (A, A) => A): A = t match {
  case Leaf(x) => f(x, z)
  case Branch(left, right) => f(fold(left, z)(f), fold(right, z)(f))
}
