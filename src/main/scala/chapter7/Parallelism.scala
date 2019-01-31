package me.fpinscala.chapter7

import java.util.concurrent._

import language.implicitConversions

object Parallelism {
  def sum(ints: IndexedSeq[Int]): Int =
    if (ints.size <= 1)
      ints.headOption getOrElse 0
    else {
      val (l,r) = ints.splitAt(ints.length/2)
      sum(l) + sum(r)
    }

  import Par._

  def sum2(ints: IndexedSeq[Int]): Int = {

    if (ints.size <= 1)
      ints.headOption.getOrElse(0)
    else {
      val (l, r) = ints.splitAt(ints.length / 2)
      val sumL: Par[Int] = Par.unit(sum2(l))
      val sumR: Par[Int] = Par.unit(sum2(r))
      Par.get(sumL) + Par.get(sumR)
    }
  }

  def sum3(ints: IndexedSeq[Int]): Par[Int] =
    if (ints.size <= 1)
      Par.unit(ints.headOption getOrElse 0)
    else {
      val (l,r) = ints.splitAt(ints.length/2)
      Par.map2(sum3(l), sum3(r))(_ + _)
    }

}

class ExecutorService {
  def submit[A](a: Callable[A]): Future[A] = ???
}

trait Callable[A] { def call: A }

trait Future[A] {
  def get: A
  def get(timeout: Long, unit: TimeUnit): A
  def cancel(evenIfRunning: Boolean): Boolean
  def isDone: Boolean
  def isCancelled: Boolean
}

object Par {
  type Par[A] = ExecutorService => Future[A]

  //def run[A](s: ExecutorService)(a: Par[A]): Future[A] = a(s)

  private case class UnitFuture[A](get: A) extends Future[A] {
    def isDone = true

    def get(timeout: Long, units: TimeUnit) = get

    def isCancelled = false

    def cancel(evenIfRunning: Boolean): Boolean = false
  }

  def unit[A](a: => A): Par[A] = (es: ExecutorService) => UnitFuture(a)

  def get[A](a: Par[A]): A = ???

  def map2[A, B, C](a: Par[A], b: Par[B])(f: (A, B) => C): Par[C] = {
    unit(f(get(a), get(b)))

    (es: ExecutorService) => {
      val l = a(es)
      val r = b(es)

      UnitFuture(f(l.get, r.get))
    }
  }

  def fork[A](a: => Par[A]): Par[A] = {
    es => es.submit(new Callable[A] {
      def call = a(es).get
    })
  }

  def lazyUnit[A](a: => A): Par[A] = fork(unit(a))
  def run[A](a: Par[A]): A = ???
}

