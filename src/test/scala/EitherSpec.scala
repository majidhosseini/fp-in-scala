package me.fpinscala

import org.scalatest._

class EitherSpec extends FlatSpec {
  "A list of either" should "right of list when items are right" in {
    val list = List(R(1), R(2), R(3))

    assert(Either.sequence(list) == R(List(1, 2, 3)))
    assert(Either.sequence(list) == Either.sequence2(list))
    assert(Either.sequence3(list) == Either.sequence2(list))
  }

  it should "left when all items aren't right" in {
    val list = List(R(1), R(2), L("test"), R(4))

    assert(Either.sequence(list) == L("test"))
    assert(Either.sequence(list) == Either.sequence2(list))
    assert(Either.sequence3(list) == Either.sequence2(list))
  }

  "traverse on list of either" should "somethings" in {
    val list = List(1, 2, 3)

    def mkStr(i: Int): Either[String, String] = if (i < 0) L("Wrong integer") else (R(i.toString))

    assert(Either.traverse(list)(mkStr) == R(List("1", "2", "3")))
  }

  it should "left when all items aren't right" in {
    val list = List(1, -2, 3)

    def mkStr(i: Int): Either[String, String] = if (i < 0) L("Wrong integer") else (R(i.toString))

    assert(Either.traverse(list)(mkStr) == L("Wrong integer"))
  }

}
