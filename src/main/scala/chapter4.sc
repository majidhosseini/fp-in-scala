import scala.{Either => _, Option => _, _}

//sealed trait Option[+A]

sealed trait Option[+A] {
  def map[B](f: A => B): Option[B] = this match {
    case Nonestan => Nonestan
    case Somestan(a) => Somestan(f(a))
  }

  def flatMap[B](f: A => Option[B]): Option[B] = this match {
    case Nonestan => Nonestan
    case Somestan(a) => f(a)
  }

  def getOrElse[B >: A](default: => B): B = this match {
    case Nonestan => default
    case Somestan(a) => a
  }

  def orElse[B >: A](ob: => Option[B]): Option[B] = this match {
    case Somestan(a) => Somestan(a)
    case Nonestan => ob
  }

  def filter(f: A => Boolean): Option[A] = this match {
    case Somestan(a) if f(a) == true => Somestan(a)
    case _ => Nonestan
  }
}

case class Somestan[+A](get: A) extends Option[A]
case object Nonestan extends Option[Nothing]
