package me.fpinscala.chapter4

import org.scalatest._
import me.fpinscala._

class PersonSpec extends FlatSpec {
  "A person" should "be defined when name and age is right things" in {
    val person = Person.mkPerson("majid", 32)

    assert(person == R(Person("majid", 32)))
  }
}
