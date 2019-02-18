import scala.{Either => _, Option => _, _}
import scala.Seq._

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

  // 4.2
  def variance(xs: Seq[Double]): Option[Double] =
    mean(xs) flatMap (m => mean(xs.map(x => math.pow(x - m, 2))))

  def lift[A,B](f: A => B): Option[A] => Option[B] = _ map f


  // 4.3
  def map2[B,C](b: Option[B])(f: (A, B) => C): Option[C] = {
    this flatMap { a2 =>
      b map { b2 =>
        f(a2, b2)
      }
    }
  }
}

def mean(xs: Seq[Double]): Option[Double] = xs.isEmpty match {
  case true  => Nonestan
  case false => Somestan(xs.sum / xs.length)
}

case class Somestan[+A](get: A) extends Option[A]
case object Nonestan extends Option[Nothing]

def Try[A](a: => A): Option[A] =
  try Somestan(a)
  catch { case e: Exception => Nonestan }

// 4.4
def sequence[A](xa: List[Option[A]]): Option[List[A]] = {
  xa match {
    case Nil => Nonestan
    case h :: t => h flatMap(a => sequence(t) map (a :: _))
  }
}

def parseInt(a: List[String]): Option[List[Int]] = {
  sequence(a.map(i => Try(i.toInt)))
}

// 4.5
def traverse[A, B](xa: List[A])(f: A => Option[B]): Option[List[B]] = {
  sequence(xa.map(a => f(a)))
}

def mean2(as: List[Double]): Option[Double] =
  as match {
    case Nil => Nonestan
    case _ => Somestan(as.foldRight(0D)(_ + _) / as.length)
  }


def variance2(as: List[Double]): Option[Double] = {
  mean(as).flatMap(m => mean(as.map(i => math.pow(i-m, 2))))
}


def map22[A,B,C](a: Option[A], b: Option[B])(f: (A, B) => C): Option[C] = {
  a flatMap {aa =>
    b map { bb =>
      f(aa, bb)
    }
  }
}

def sequence2[A](a: List[Option[A]]): Option[List[A]] = {
  a match {
    case Nil => Nonestan
    case x :: xs => x flatMap(xx => sequence2(xs).map(xx :: _))
  }
}

def traverse2[A, B](as: List[A])(f: A => Option[B]): Option[List[B]] = {
  sequence2(as.map(f(_)))
}

def sequence3[A](a: List[Option[A]]): Option[List[A]] = {
  a match {
    case Nil => Nonestan
    case x :: xs => x.flatMap(xx => sequence3(xs).map(xx :: _))
  }
}

val l = List(Somestan(1), Somestan(1), Somestan(3), Nonestan, Somestan(5), Nonestan, Somestan(10))
sequence(l)
