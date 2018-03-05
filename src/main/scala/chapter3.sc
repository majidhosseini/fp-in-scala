import List._

sealed trait List[+A]
case object Nil extends List[Nothing]
case class Cons[+A](head: A, tail: List[A]) extends List[A]

def tail[A](l: List[A]): List[A] = {
  l match {
    case Nil => Nil
    case Cons(_, xs) => xs
  }
}

def setHead[A](ds: List[A], head: A): List[A] = {
  ds match {
    case Nil => List(head)
    case _ => Cons(head, ds)
  }
}

def drop[A](l: List[A], n: Int): List[A] = {
  l match {
    case Nil => Nil
    case Cons(_, xs) if n == 1 => xs
    case Cons(_, xs) => drop(xs, n - 1)
  }
}

def dropWhile[A](l: List[A], f: A => Boolean): List[A] = {
  l match {
    case Nil => Nil
    case Cons(x, xs) if f(x) => xs
    case Cons(_, xs) => dropWhile(xs, f)
  }
}


def append[A](a1: List[A], a2: List[A]): List[A] = {
  a1 match {
    case Nil => a2
    case Cons(h, t) => Cons(h, append(t, a2))
  }
}

def init[A](l: List[A]): List[A] = {
  l match {
    case Cons(x,Nil) => Nil
    case Nil => Nil
    case Cons(x, xs) => Cons(x, init(xs))
  }
}

def foldRight[A,B](as: List[A], z: B)(f: (A, B) => B): B = {
  as match {
    case Nil => z
    case Cons(x, xs) => f(x, foldRight(xs, z)(f))
  }
}

def length[A](as: List[A]): Int = foldRight(as, 0)((_, count) => count + 1)

def foldLeft[A, B](as: List[A], z: B)(f: (B, A) => B): B = {
  as match {
    case Nil => z
    case Cons(x, xs) => foldLeft(xs, f(z, x))(f)
  }
}

// Exercise 3.12
def recursive[A](as: List[A]): List[A] = foldLeft(as, Nil:List[A])((x, y) => Cons(y, x))

// Exercise 2.13
//def foldLeft2[A, B](as: List[A], z: B)(f: (B, A) => B): B =
//  as match {
//    case Nil => z
//    case Cons(x, xs) => foldRight(xs, x)(f(z, x))
//  }
//
//foldLeft2(List(1,2,3,4), Nil:List[Int])((t, h) => Cons(h, t))

def appendRight[A](a1: List[A], a2: List[A]): List[A] = foldRight(a1, a2)(Cons(_,_))
appendRight(List(1,2,3,4),List(5,6))
def appendLeft[A](a1: List[A], a2: List[A]): List[A] = foldLeft(a1, a2)((a, b) => Cons(b, a))
appendLeft(List(1,2,3,4),List(5,6))

object List {
  def sum(l: List[Int]): Int = l match {
    case Nil => 0
    case Cons(x, xs) => x + sum(xs)
  }

  def product(ds: List[Double]): Double = ds match {
    case Nil => 1.0
    case Cons(0.0, _) => 0.0
    case Cons(x, xs) => x * product(xs)
  }

  def sum2(ns: List[Int]) = foldRight(ns, 0)((x, y) => x + y)
  def product2(ns: List[Int]) = foldRight(ns, 1.0)(_ * _)

  def apply[A](as: A*): List[A] =
    if (as.isEmpty) Nil
    else Cons(as.head, apply(as.tail: _*))
}

val ex1: List[Double] = Nil
val ex2: List[Int] = Cons(1, Nil)
val ex3: List[String] = Cons("a", Cons("b", Nil))

// Exercise 1
List(1,2,3,4,5) match {
  case Cons(x, Cons(2, Cons(4, _))) => x
  case Nil => 42
  case Cons(x, Cons(y, Cons(3, Cons(4, _)))) => x + y
  case Cons(h, t) => h + sum(t)
  case _ => 101
}

tail(ex3)

val a = List(1,2,3,4, 10)
drop(a, 3)

init(a)

foldRight(a, Nil:List[Int])(Cons(_,_))
recursive(a)
length(a)

val str = List("m", "a", "j", "i", "d")
foldRight(List(2,10), 1)(_ / _)
foldLeft(List(2, 10), 1)(_ / _)
10/2/1
1/2/10

