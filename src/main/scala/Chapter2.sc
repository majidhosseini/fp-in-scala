import scala.annotation.tailrec
import scala.collection.immutable.::

def fib(n: Int) = {
  @tailrec
  def go(num1: Int, prev: Int, acc: Int): Double = {
    if (num1 == 0) prev
    else go(num1 - 1, acc, prev + acc)
  }

  go(n, 0, 1)
}

fib(1)
fib(2)
fib(3)
fib(4)
fib(5)
fib(6)
fib(7)
fib(8)



def isSorted[A](as: Array[A], gt: (A,A) => Boolean): Boolean = {
  @annotation.tailrec
  def go(n: Int): Boolean =
    if (n >= as.length-1) true
    else if (gt(as(n), as(n+1))) go(n+1)
    else false

  go(0)
}

def compare(a: Int, b: Int): Boolean = a < b
val arr = Seq(1, 3, 4, 7).toArray

isSorted(arr, compare)



def curry[A,B,C](f: (A, B) => C): A => (B => C) = a => b => f(a, b)

def uncurry[A,B,C](f: A => B => C): (A, B) => C = (a, b) => f(a)(b)

def compose[A,B,C](f: B => C, g: A => B): A => C = a => f(g(a))