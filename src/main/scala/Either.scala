package me.fpinscala

trait Either[+E, +A] {
  def map[B](f: A => B): Either[E, B] = {
    this match {
      case R(a) => R(f(a))
      case L(e) => L(e)
    }
  }

  def flatMap[EE >: E, B](f: A => Either[EE, B]): Either[EE, B] =
    this match {
      case R(a) => f(a)
      case L(e) => L(e)
    }

  def orElse[EE >: E, B >: A](b: => Either[EE, B]): Either[EE, B] =
    this match {
      case R(_) => b
      case L(e) => L(e)
    }

  def map2[EE >: E, B, C](b: Either[EE, B])(f: (A, B) => C): Either[EE, C] = {
    this match {
      case R(a) => b match {
        case R(b) => R(f(a, b))
        case L(e) => L(e)
      }
      case L(e) => L(e)
    }
  }

  def map3[EE >: E, B, C](b: Either[EE, B])(f: (A, B) => C): Either[EE, C] = {
    this flatMap { a =>
      b map { b1 =>
        f(a, b1)
      }
    }
  }

  def map4[EE >: E, B, C](b: Either[EE, B])(f: (A, B) => C): Either[EE, C] = {
    for {
      a <- this
      b1 <- b
    } yield f(a, b1)
  }
}

case class L[+E](get: E) extends Either[E,Nothing]
case class R[+A](get: A) extends Either[Nothing,A]

object Either {
  def mean(xs: IndexedSeq[Double]): Either[String, Double] =
    if (xs.isEmpty)
      L("mean of empty list!")
    else
      R(xs.sum / xs.length)

  def safeDiv(x: Int, y: Int): Either[Exception, Int] =
    try R(x / y)
    catch { case e: Exception => L(e) }

  def Try[A](a: => A): Either[Exception, A] =
    try R(a)
    catch { case e: Exception => L(e) }

  def sequence[E, A](es: List[Either[E, A]]): Either[E, List[A]] = {
    es match {
      case Nil => R(Nil)
      case x :: xs => x.flatMap(xx => sequence(xs).map(xx :: _))
    }
  }

  def traverse[E, A, B](as: List[A])(f: A => Either[E, B]): Either[E, List[B]] = sequence(as.map(f))

  // traverse(es)(e => e) !!!!!!!!!!!!!!!!!!!!!!!!!!
  def sequence2[E, A](es: List[Either[E, A]]): Either[E, List[A]] = traverse(es)(e => e)

  def sequence3[E, A](es: List[Either[E, A]]): Either[E, List[A]] = {
    es.foldRight[Either[E, List[A]]](R(Nil))((a, b) => a.flatMap(aa => b.map(aa :: _)))
  }

  def traverse2[E, A, B](es: List[A])(f: A => Either[E, B]): Either[E, List[B]] = {
    es.foldRight[Either[E,List[B]]](R(Nil))((a, b) => f(a).map2(b)(_ :: _))
  }

}


