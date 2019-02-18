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
def fold[A,B](t: Tree[A])(f: A => B)(g: (B,B) => B): B = t match {
  case Leaf(a) => f(a)
  case Branch(left, right) => g(fold(left)(f)(g), fold(right)(f)(g))
}

def size2[A](t: Tree[A]): Int = fold(t)(_ => 1)(_ + _ + 1)

def maximum2[A](t: Tree[A])(f: (A,A) => A): A = fold(t)(a => a)((a, b) => f(a, b))

def depth2[A](t: Tree[A]): Int = fold(t)(_ => 0)((a, b) => 1 + (a max b))
