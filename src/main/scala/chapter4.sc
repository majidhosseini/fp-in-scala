import scala.{Either => _, Option => _, _}

sealed trait Option[+A] {
  def map[B](f: A => B): Option[B] = this match {
    case Nonestan => ???
    case Somestan(a) => Somestan(f(a))
  }

  def flatMap[B](f: A => Option[B]): Option[B] = ???


  def getOrElse[B >: A](default: => B): B = ???
  def orElse[B >: A](ob: => Option[B]): Option[B] = ???
  def filter(f: A => Boolean): Option[A] = ???
}

case class Somestan[+A](get: A) extends Option[A]
case object Nonestan extends Option[Nothing] {
  def get = throw new UnsupportedOperationException("get not defined on Absent")
}
