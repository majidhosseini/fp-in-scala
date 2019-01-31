package me.fpinscala.chapter7

import org.scalatest.FlatSpec

class ParallelismSpec extends FlatSpec {
  "to sum as parallelism" should "return a par result" in {
    val list = IndexedSeq(1,2,3,4,5)

    Parallelism.sum(list) === 15
  }
}
