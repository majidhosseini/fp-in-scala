import List._

sealed trait List[+A]
case object Nil extends List[Nothing]
case class Cons[+A](head: A, tail: List[A]) extends List[A]


object List {
  def sum(l: List[Int]): Int = l match {
    case Nil => 0
    case Cons(x, xs) => x + sum(xs)
  }

  //3.7
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

def foldRight[A,B](as: List[A], z: B)(f: (A, B) => B): B = {
  as match {
    case Nil => z
    case Cons(x, xs) => f(x, foldRight(xs, z)(f))
  }
}

def foldLeft[A, B](as: List[A], z: B)(f: (B, A) => B): B = {
  as match {
    case Nil => z
    case Cons(x, xs) => foldLeft(xs, f(z, x))(f)
  }
}

//3.2
def tail[A](l: List[A]): List[A] = {
  l match {
    case Nil => Nil
    case Cons(_, xs) => xs
  }
}

//3.3
def setHead[A](ds: List[A], head: A): List[A] = {
  ds match {
    case Nil => List(head)
    case _ => Cons(head, ds)
  }
}

//3.4
def drop[A](l: List[A], n: Int): List[A] = {
  l match {
    case Nil => Nil
    case Cons(_, xs) if n == 1 => xs
    case Cons(_, xs) => drop(xs, n - 1)
  }
}

//3.5
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

//3.6
def init[A](l: List[A]): List[A] = {
  l match {
    case Cons(x,Nil) => Nil
    case Nil => Nil
    case Cons(x, xs) => Cons(x, init(xs))
  }
}

//3.7
//def product[Int](l: List[Int]): Int = {
//  foldRight(l, 1)(_ * _)
//}

// 3.11
//def sum[Int](l: List[Int]): Int = foldRight(l, 1)((a, b) => a + b)




// 3.9
def length[A](l: List[A]): Int = {
  foldRight(l, 0)((_, b) => b + 1)
}

//3.10
//def foldLeftTail[A, B](as: List[A], z: B)(f: (B, A) => B): B = {
//  as match {
//    case Nil => z
//    case Cons(x, xs) => foldLeftTail(xs, f(x, z))(f)
//  }
//}


//3.12
//def reverse[A](as: List[A]): List[A] = {
//  as match {
//    case Nil => Nil
//    case Cons(x, xs) => Cons(x )
//  }
//
//}

def reverseWithFR[A](as: List[A]): List[A] = {
  foldRight(as, Nil)((a, b) => Cons(b, a))
}


val listTest = List(1,2,3,4,5,6,7,8,9)
//product(listTest, 1)(_*_)


foldRight(listTest, Nil:List[Int])(Cons(_,_))
length(listTest)

reverseWithFR(listTest)

