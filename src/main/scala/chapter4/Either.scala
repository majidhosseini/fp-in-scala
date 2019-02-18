package me.fpinscala.chapter4

trait Either[+E, +A] {
  def map[B](f: A => B): Either[E, B] = this match {
    case Right(a) => Right(f(a))
    case Left(e) => Left(e)
  }

  def flatMap[EE >: E, B](f: A => Either[EE, B]): Either[EE, B] = this match {
    case Right(a) => f(a)
    case Left(e) => Left(e)
  }

  def orElse[EE >: E, B >: A](b: => Either[EE, B]): Either[EE, B] = this match {
    case Right(a) => Right(a)
    case Left(_) => b
  }

  def map2[EE >: E, B, C](b: Either[EE, B])(f: (A, B) => C): Either[EE, C] = {
//    this match {
//      case Right(a) => b match {
//        case Right(bb) => Right(f(a, bb))
//        case Left(e) => Left(e)
//      }
//      case Left(e) => Left(e)
//    }
    for {
      a <- this
      bb <- b
    } yield f(a, bb)
  }
}

case class Left[+E](get: E) extends Either[E, Nothing]
case class Right[+A](get: A) extends Either[Nothing, A]

object Either {
  def sequence[E, A](es: List[Either[E, A]]): Either[E, List[A]] = {
    def go(es: List[Either[E, A]], as: List[A]): Either[E, List[A]] = {
      es match {
        case Nil => Right(as)
        case h :: t => h match {
          case Right(a) => go(t, a :: as)
          case Left(e) => Left(e)
        }
      }
    }

    go(es, Nil)
  }

  def traverse[E, A, B](as: List[A])(f: A => Either[E, B]): Either[E, List[B]] = as match {
    case Nil => Right(Nil)
    case h :: t => (f(h).map2(traverse(t)(f)))(_ :: _)
  }

  def sequenceWithTraverse[E, A](es: List[Either[E, A]]): Either[E, List[A]] = traverse(es)(e => e)
}
