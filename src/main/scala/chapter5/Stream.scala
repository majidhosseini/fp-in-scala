package me.fpinscala.chapter5

import me.fpinscala.chapter3._

sealed trait Stream[+A] {

  def headOption: Option[A] = this match {
    case Empty => None
    case Cons(h, _) => Some(h())
  }

  // 5.1
  def toList: Li[A] = {
    def go(s: Stream[A]): Li[A] = {
      s match {
        case Empty => N
        case Cons(h, t) => C(h(), go(t()))
      }
    }

    go(this)
  }

  // 5.2
  def take(n: Int): Stream[A] = {
    def go(s: Stream[A], n: Int): Stream[A] = {
      s match {
        case Cons(h, t) if n > 0 => Cons(h, () => go(t(), n - 1))
        case _ => Stream.empty
      }
    }

    go(this, n)
  }

  // 5.2
  def drop(n: Int): Stream[A] = {
    def go(s: Stream[A], n: Int): Stream[A] = {
      s match {
        case Cons(_, t) if n > 0 => go(t(), n-1)
        case Cons(h, t) => Cons(h, t)
        case _ => Stream.empty
      }
    }

    go(this, n)
  }

  // 5.3
  def takeWhile(p: A => Boolean): Stream[A] = {
    def go(s: Stream[A]): Stream[A] = {
      s match {
        case Cons(h, t) if (p(h())) => Cons(h, () => go(t()))
        case _ => Empty
      }
    }

    go(this)
  }

  def exist(p: A => Boolean): Boolean = this match {
    case Cons(h, t) => p(h()) || t().exist(p)
    case _ => false
  }

  def foldRight[B](z: => B)(f: (A, => B) => B): B = this match {
    case Cons(h, t) => f(h(), t().foldRight(z)(f))
    case _ => z
  }

  def existWithFoldRight(p: A => Boolean): Boolean =
    this.foldRight(false)((a, b) => p(a) || b)

  // 5.4
  def forAll(p: A => Boolean): Boolean =
    this.foldRight(true)((a, b) => p(a) && b)

  // 5.5
  def takeWhileWithFoldRight(p: A => Boolean): Stream[A] = {
    val em: Stream[A] = Empty
    this.foldRight(em)((a, b) => if (p(a)) Cons(() => a, () => b) else Empty)
  }

  // 5.6
  def headOptionWithFoldRight: Option[A] = {
    val none: Option[A] = None
    this.foldRight(none)((a, _) => Some(a))
  }

  // 5.7
  def map[B](f: A => B): Stream[B] = {
    val em: Stream[B] = Empty
    this.foldRight(em)((h, t) => Cons(() => f(h), () => t))
  }

  // 5.7
  def filter(p: A => Boolean): Stream[A] = {
    val em: Stream[A] = Empty
    this.foldRight(em)((h, t) => if (p(h)) Cons(() => h, () => t) else t)
  }

  // 5.7
  def append[B >: A](s: => Stream[B]): Stream[B] = {
    this.foldRight(s)((h, t) => Cons(() => h, () => t))
  }

  // 5.7
  def flatMap[B](f: A => Stream[B]): Stream[B] = {
    val em: Stream[B] = Empty
    this.foldRight(em)((h, t) => f(h) append t)
  }

  def find(p: A => Boolean): Option[A] = this.filter(p).headOption

  // 5.13
  def mapWithUnfold[B](f: A => B): Stream[B] = {
    Stream.unfold(this)(h =>
      h match {
        case Cons(h1, t1) => Some((f(h1()), t1()))
        case _ => None
      }
    )
  }

  // 5.13
  def takeWithUnfold(n: Int): Stream[A] = {
    Stream.unfold((this, n))(couple =>
      couple._1 match {
        case Cons(h, t) if couple._2 > 0 => Some((h(), (t(), couple._2 - 1)))
        case _ => None
      }
    )
  }

  // 5.13
  def takeWhileWithUnfold(p: A => Boolean): Stream[A] = {
    Stream.unfold((this))(s =>
      s match {
        case Cons(h, t) if p(h()) => Some((h(), t()))
        case _ => None
      }
    )
  }

  // 5.13
  def zipWith[B,C](s2: Stream[B])(f: (A,B) => C): Stream[C] = {
    Stream.unfold((this, s2)) {
      case (Cons(h1, t1), Cons(h2, t2)) => Some((f(h1(), h2()), (t1(), t2())))
      case _ => None
    }
  }

  // 5.13
  def zipAll[B](s2: Stream[B]): Stream[(Option[A], Option[B])] = {
    Stream.unfold((this, s2)) {
      case (Cons(h1, t1), Cons(h2, t2)) => Some((Some(h1()), Some(h2())), (t1(), t2()))
      case (Cons(h1, t1), _) => Some((Some(h1()), None), (t1(), Empty))
      case (Empty, Cons(h2, t2)) => Some((None, Some(h2())), (Empty, t2()))
      case _ => None
    }
  }

  // 5.14
  def startsWith[A](s: Stream[A]): Boolean = {
    this.zipAll(s).takeWhile(!_._2.isEmpty).forAll(x => x._1 == x._2)
  }

  def tails: Stream[Stream[A]] = {
    Stream.unfold(this){ case Cons(h, t) =>
      Some((Cons(h, t), t()))
    case _ => None
    }
  }

  def hasSubsequence[A](s: Stream[A]): Boolean = tails.exist(_.startsWith(s))

  def scanRight[B](z: => B)(f: (A, => B) => B): Stream[B] = {
    this.tails.map(r => r.foldRight(z)(f)).append(Stream(z))
  }

  def scanRightViaRecursive[B](z: => B)(f: (A, => B) => B): Stream[B] = {

    val temp = this.foldRight(z)(f)

   ???
  }


}

case object Empty extends Stream[Nothing]
case class Cons[+A](h: () => A, t: () => Stream[A]) extends Stream[A]

object Stream {
  def cons[A](hd: => A, t1: => Stream[A]): Stream[A] = {
    lazy val head = hd
    lazy val tail = t1
    Cons(() => head, () => tail)
  }

  def empty[A]: Stream[A] = Empty

  def apply[A](as: A*): Stream[A] = {
    if (as.isEmpty) empty else cons(as.head, apply(as.tail: _*))
  }

  val ones: Stream[Int] = Stream.cons(1, ones)

  // 5.8
  def constant[A](a: A): Stream[A] = Stream.cons(a, constant(a))

  // 5.9
  def from(n: Int): Stream[Int] = Stream.cons(n, from(n+1))

  // 5.10
  def fibs: Stream[Int] = {
    def go(first: Int, second: Int) : Stream[Int] = {
      Stream.cons(first, go(second, first + second))
    }

    go(0,1)
  }

  // 5.11
  def unfold[A, S](z: S)(f: S => Option[(A, S)]): Stream[A] = {
    f(z) match {
      case None => empty
      case Some((a, s)) => Stream.cons(a, unfold(s)(f))
    }
  }

  def unfoldViaFold[A, S](z: S)(f: S => Option[(A, S)]): Stream[A] = ???
  def unfoldViaMap[A, S](z: S)(f: S => Option[(A, S)]): Stream[A] = ???

  // 5.12 fibs, from, constant, ones with unfold
  def fibsWithUnfold: Stream[Int] = unfold((0, 1))(x => Some(x._1, (x._2, x._1 + x._2)))
  def fromWithUnfold(n: Int): Stream[Int] = unfold(n)(x => Some(x, x + 1))
  def constantWithUnfold[A](a: A): Stream[A] = unfold(a)(x => Some(x, x))
  def onesWithUnfold: Stream[Int] = unfold(1)(_ => Some(1, 1))


}
