package me.fpinscala.chapter6

trait RNG {
  def nextInt: (Int, RNG)
}

case class SimpleRNG(seed: Long) extends RNG {
  override def nextInt: (Int, RNG) = {
    val newSeed = (seed * 0x5DEECE66DL + 0xBL) & 0xFFFFFFFFFFFFL
    val nextRNG =  SimpleRNG(newSeed)
    val n = (newSeed >>> 16).toInt
    (n, nextRNG)
  }

  def randomPair(rng: RNG): ((Int,Int), RNG) = {
    val (i1,rng2) = rng.nextInt
    val (i2,rng3) = rng2.nextInt
    ((i1,i2), rng3)
  }

  def nonNegativeInt(rng: RNG): (Int, RNG) = {
    def go(rng: RNG): (Int, RNG) = {
      val nextInt = rng.nextInt

      if (Int.MaxValue <= nextInt._1 || Int.MinValue >= nextInt._1) go(rng) else go(nextInt._2)
    }

    go(rng)
  }

  // 6.2
  def double(rng: RNG): (Double, RNG) = {
    val (i, r) = nonNegativeInt(rng)
    (i / (Int.MaxValue.toDouble + 1), r)
  }

  def intDouble(rng: RNG): ((Int,Double), RNG) = {
    val (i, r1) = rng.nextInt
    val (d, r2) = double(r1)

    ((i, d), r2)
  }

  def doubleInt(rng: RNG): ((Double,Int), RNG) = {
    val ((i, d), r) = intDouble(rng)
    ((d, i), r)
  }

  def double3(rng: RNG): ((Double,Double,Double), RNG) = {
    val (d1, r1) = double(rng)
    val (d2, r2) = double(r1)
    val (d3, r3) = double(r2)

    ((d1,d2,d3), r3)
  }

  def ints(count: Int)(rng: RNG): (List[Int], RNG) = {
    if (count == 0) (Nil, rng)
    else {
      val (i, r) = rng.nextInt
      val (xs, r2) = ints(count-1)(r)
      (i :: xs, r2)
    }
  }

  def intsRec(count: Int)(rng: RNG): (List[Int], RNG) = {
    def go(count: Int, r: RNG, l: List[Int]): (List[Int], RNG) = {
      if (count == 0) (l, r)
      else {
        val (i, newR) = r.nextInt
        go(count - 1, newR, i :: l)
      }
    }

    go(count, rng, Nil)
  }

  type Rand[+A] = RNG => (A, RNG)

  val int: Rand[Int] = _.nextInt

  def unit[A](a: A): Rand[A] = rng => (a, rng)

  def map[A,B](s: Rand[A])(f: A => B): Rand[B] = rng => {
      val (a, rng2) = s(rng)
      (f(a), rng2)
    }

  def nonNegativeEven: Rand[Int] = map(nonNegativeInt)(i => i - i % 2)


}
