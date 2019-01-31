package me.fpinscala.chapter5

import org.scalatest._
import org.scalatest.Matchers._
import me.fpinscala.chapter3.Li

class StreamSpec extends FlatSpec {
  "toList action on stream" should "create a valid list" in {
    val list = Li(1,2,3,4,5)

    val stream = Stream(1,2,3,4,5)

    assert(stream.toList === list)
  }

  "take action on stream" should "return n first items" in {
    val stream = Stream(1,2,3,4,5)

    assert(stream.take(3).toList === Li(1,2,3))
    assert(stream.takeWithUnfold(3).toList === Li(1,2,3))
  }

  "drop action on stream" should "dropped n first item" in {
    val stream = Stream(1,2,3,4,5)

    stream.drop(3).toList === Li(4,5)
  }

  "takeWhile on stream" should "returning all starting elements of a stream that match the given predicate" in {
    val stream = Stream(1,2,3,4,5)

    assert(stream.takeWhile(_ < 4).toList === Li(1,2,3))
    assert(stream.takeWhileWithFoldRight(_ < 4).toList === Li(1,2,3))
    assert(stream.takeWhileWithUnfold(_ < 4).toList === Li(1,2,3))
  }

  it should "return all elements" in {
    val stream = Stream(1,2,3,4,5)
    assert(stream.takeWhile(_ < 6).toList === Li(1,2,3,4,5))
  }

  "forAll on stream" should "check all elements" in {
    val stream = Stream(1,2,3,4,5)
    assert(stream.forAll(_ < 10) === true)
  }

  it should "false if like this" in {
    val stream = Stream(1,2,3,4,5)

    assert(stream.forAll(_ < 4) === false)
  }

  "headOptionWithFoldRight on stream" should "return first element" in {
    val stream = Stream(1,3,2,6,6)
    assert(stream.headOptionWithFoldRight === Some(1))
  }

  it should "return None" in {
    val stream = Stream.empty

    assert(stream.headOptionWithFoldRight === None)
  }

  "map on stream" should "return sth" in {
    val stream = Stream(1,2,3,4,5)

    assert(stream.map(_ * 2).toList === Li(2,4,6,8,10))
    assert(stream.mapWithUnfold(_ * 2).toList === Li(2,4,6,8,10))
  }

  "filter on stream" should "return odd elements" in {
    val stream = Stream(1,2,3,4,5)

    assert(stream.filter(_ % 2 == 1).toList === Li(1,3,5))
  }

  "append on stream" should "return two streams elements" in {
    val stream = Stream(1,2,3,4,5)
    val stream2 = Stream(6)

    assert(Stream.ones.take(5).toList === Li(1,1,1,1,1))
    assert(Stream.onesWithUnfold.take(5).toList === Li(1,1,1,1,1))

    assert(stream.append(stream2).toList === Li(1,2,3,4,5,6))
  }

  "constant on stream" should "return infinitive stream elements" in {
    assert(Stream.constant('b').take(3).toList === Li('b','b','b'))
    assert(Stream.constantWithUnfold('b').take(3).toList === Li('b','b','b'))

    assert(Stream.from(25).take(3).toList === Li(25,26,27))
    assert(Stream.fromWithUnfold(25).take(3).toList === Li(25,26,27))
  }

  "fibs on stream" should "return infinitive stream elements of fibonacci" in {
    assert(Stream.fibs.take(10).toList === Li(0,1,1,2,3,5,8,13,21,34))

    assert(Stream.fibsWithUnfold.take(10).toList === Li(0,1,1,2,3,5,8,13,21,34))
  }

  "unfold on stream" should "return infinitive stream elements" in {
    val unfold = Stream.unfold(5)(x => Some(x * 2, x + 1))

    assert(unfold.take(5).toList === Li(10,12,14,16,18))
  }

  "zipWith on stream" should "return a stream with operated with another" in {
    val s1 = Stream(1,2,3)
    val s2 = Stream(7,0,2,6)

    val zipped = s1.zipWith(s2)(_ + _)

    assert(zipped.toList === Li(8,2,5))
  }

  "zipAll on stream" should "with two stream has same count element" in {
    val s1 = Stream(1,2,3)
    val s2 = Stream(7,0,2)

    val zipped = s1.zipAll(s2)

    assert(zipped.toList === Li(
      (Some(1), Some(7)),
      (Some(2), Some(0)),
      (Some(3), Some(2))
    ))
  }

  it should "return longest length when first is bigger than" in {
    val s1 = Stream(1,2,3,6,9)
    val s2 = Stream(7,0,2)

    val zipped = s1.zipAll(s2)

    assert(zipped.toList === Li(
      (Some(1), Some(7)),
      (Some(2), Some(0)),
      (Some(3), Some(2)),
      (Some(6), None),
      (Some(9), None)
    ))
  }

  it should "return longest length when second is bigger than" in {
    val s1 = Stream(1,2,3)
    val s2 = Stream(7,0,2,8,3)

    val zipped = s1.zipAll(s2)

    assert(zipped.toList === Li(
      (Some(1), Some(7)),
      (Some(2), Some(0)),
      (Some(3), Some(2)),
      (None, Some(8)),
      (None, Some(3))
    ))
  }

  "startsWith" should "return true for this" in {
    val s1 = Stream(1,2,3)
    val s2 = Stream(1,2,3,8,3)

    assert(s2.startsWith(s1) === true)
  }

  it should "return false for this" in {
    val s1 = Stream(1,2,3,8,3,4,9,1,2,3,8,4,3)
    val s2 = Stream(1,2,3,8,4)

    assert(s2.startsWith(s1) === false)
  }

  "tails" should "return submergence" in {
    val s = Stream(1,2,3)

    assert(s.tails.map(r => r.toList).toList.toString === "C(C(1,C(2,C(3,N))),C(C(2,C(3,N)),C(C(3,N),N)))")
  }

  "scanRight" should "return this" in {
    assert(Stream(1,2,3).scanRight(0)(_ + _).toList == Li(6,5,3,0))
  }

}
