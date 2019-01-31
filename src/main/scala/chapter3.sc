import List._

sealed trait List[+A]
case object Nil extends List[Nothing]
case class Cons[+A](head: A, tail: List[A]) extends List[A]

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
def reverse[A](as: List[A]): List[A] = foldLeft(as, Nil:List[A])((x, y) => Cons(y, x))


// 3.13
def foldLeftWithFoldRight[A, B](as: List[A], z: B)(f: (A,B) => B): B = {
  as match {
    case Nil => z
    case Cons(x, xs) => foldRight(xs, f(x, z))((a, _) => f(a, z))
  }
}

// identity a => a
def fr2[A, B](z: B, as: List[A])(f: (A, B) => B): B = {
  foldLeft(as, identity[B](_))((acc, a) => newZ => acc(f(a, newZ)))(z)
}

//def test[A, B](as: List[A], z: B)(f: (A,B) => B): B


fr2(1, List(10,2,300))(_ + _)

// Exercise 3.13
//def foldLeft2[A, B](as: List[A], z: B)(f: (B, A) => B): B =
//  as match {
//    case Nil => z
//    case Cons(x, xs) => foldRight(xs, x)(f(z, x))
//  }
//
//foldLeft2(List(1,2,3,4), Nil:List[Int])((t, h) => Cons(h, t))

def foldRightWithFoldLeft[A,B](as: List[A], s: B)(f: (A,B) => B): B =
  foldLeft(as, identity[B](_))((a, b) => x => a(f(b,x)))(s)

//def foldRightWithFoldLeft[A,B](as: List[A], s: B)(f: (A,B) => B): B =
//  foldLeft(as, f)((g, a) => g(a, f(a,s)))

















// 3.14
def appendRight[A](a1: List[A], a2: List[A]): List[A] = foldRight(a1, a2)(Cons(_,_))
appendRight(List(1,2,3,4),List(5,6))
def appendLeft[A](a1: List[A], a2: List[A]): List[A] = foldLeft(reverse(a1), a2)((a, b) => Cons(b, a))
appendLeft(List(1,2,3,4),List(5,6))


//3.15
def concatenates[A](as: List[List[A]]): List[A] = {
  as match {
    case Nil => Nil
    case Cons(x, xs) => appendLeft(x, concatenates(xs))
  }
}


//def flatten[A](l: List[List[A]]): List[A] = {
//  foldRight[List[A], List[A]](l, Nill) { (curr, acc) =>
//    foldRight(curr, acc)((curr2, acc2) => Cons(curr2, acc2))
//  }
//}


concatenates(List(List(1,2,3), List(4,5,6), List(7,8,9)))


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

// 3.16
def transformer(l: List[Int]): List[Int] = l match {
  case Nil => Nil
  case Cons(x, xs) => Cons(x + 1, transformer(xs))
}
println(">>>>>>>>>>>>>>>>> 3.16")
transformer(List(1,2,3,4,5))

def tran2(l: List[Int]): List[Int] =


// 3.17
def toStringConverter(l: List[Double]): List[String] = l match {
  case Nil => Nil
  case Cons(x, xs) => Cons(x.toString, toStringConverter(xs))
}
println(">>>>>>>>>>>>>>>>> 3.17")
toStringConverter(List(1.1,2.0,3.58,4.01,5))

























// 3.18
def map[A, B](as: List[A])(f: A => B): List[B] = as match {
  case Nil => Nil
  case Cons(x, xs) => Cons(f(x), map(xs)(f))
}
println(">>>>>>>>>>>>>>>>> 3.18")
def toStringX(d: Double): String = d.toString
map(List(1.1,2.0,3.58,4.01,5))(toStringX)





// 3.19

// write with foldRight

def filter2[A](as: List[A])(f: A => Boolean): List[A] = {

}


// 3.20
def flatMap[A, B](as: List[A])(f: A => List[B])




// 3.19
def filter[A](as: List[A])(f: A => Boolean): List[A] = as match {
  case Nil => Nil
  case Cons(x, xs) if (f(x) == true) => Cons(x, filter(xs)(f))
  case Cons(_, xs) => filter(xs)(f)
}

println(">>>>>>>>>>>>>>>>> 3.19")
def isEvenNumber(i: Int) = i % 2 == 0
filter(List(1,2,3,4,5,6))(isEvenNumber)

// 3.20
def flatMap[A,B](as: List[A])(f: A => List[B]): List[B] = as match {
  case Nil => Nil
  case Cons(x, xs) => append(f(x),flatMap(xs)(f))
}
println(">>>>>>>>>>>>>>>>> 3.20")
flatMap(List(1,2,3))(i => List(i, i))

// 3.21
def filter2[A](as: List[A])(f: A => Boolean): List[A] = flatMap(as)(a =>
  f(a) match {
    case true => Cons(a, Nil)
    case false => Nil
  }
)
println(">>>>>>>>>>>>>>>>> 3.21")
//filter2(List(1,2,3,4,5,6))(i => i % 2 == 0)

// 3.22 and 3.23
def zipWith[A](as: List[A], bs: List[A])(f: (A,A) => A): List[A] = (as, bs) match {
  case (xs, Nil) => Nil
  case (Nil, ys) => Nil
  case (Cons(x, xs), Cons(y, ys)) => Cons(f(x, y), zipWith(xs, ys)(f))
}
println(">>>>>>>>>>>>>>>>> 3.22 and 3.23")
zipWith(List(1,2,3),List(4,5,6))((i,j) => i + j)


// 3.24
def hasSubsequence[A](sup: List[A], sub: List[A]): Boolean = (sup, sub) match {
  case (Nil, Nil) => true
  case (ps, Nil) => true
  case (Nil, bs) => false
  case (Cons(p, ps), Cons(b, bs)) if (p == b) => hasSubsequence(ps, bs)
  case (Cons(p, ps), Cons(b, bs)) if (p != b) => hasSubsequence(ps, sub)
}
val a1 = List(1,2,3,4,5)
val b1 = List(1,2,3)
val b2 = List(3)
val b3 = List(3,4,5)
println(">>>>>>>>>>>>>>>>> 3.24")
hasSubsequence(a1,b1)
hasSubsequence(a1,b2)
hasSubsequence(a1,b3)
hasSubsequence(a1,List(4,5,6))


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
reverse(a)
length(a)

val str = List("m", "a", "j", "i", "d")
foldRight(List(2,10), 1)(_ / _)
foldLeft(List(2, 10), 1)(_ / _)
10/2/1
1/2/10





