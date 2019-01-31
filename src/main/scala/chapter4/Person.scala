package me.fpinscala

case class Person(name: String, age: Int)
sealed class Name(val value: String)
sealed class Age(val value: Int)


object Person {

  def mkName(name: String): me.fpinscala.Either[String, Name] =
    if (name == "" || name == null) L("Name is empty.")
    else R(new Name(name))

  def mkAge(age: Int): Either[String, Age] =
    if (age < 0) L("Age is out of range.")
    else R(new Age(age))

  def mkPerson(name: String, age: Int): Either[String, Person] =
    //mkName(name).map2(mkAge(age))(Person(_, _))
    mkName(name).map2(mkAge(age))((n, a) => Person(n.value, a.value))

  def mkPerson2(name: String, age: Int): Either[List[String], Person] = {
    val nn = mkName(name)
    val aa = mkAge(age)

    nn match {
      case R(n) => ???
      case L(e) => ???
    }
  }
}


