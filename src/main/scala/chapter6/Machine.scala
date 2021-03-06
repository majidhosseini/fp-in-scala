package me.fpinscala.chapter6

import State._

sealed trait Input
case object Coin extends Input
case object Turn extends Input

case class Machine(
  locked: Boolean,
  candies: Int,
  coins: Int
)
