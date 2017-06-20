package com.truelaurel.algorithm.game

sealed trait Outcome[+P]

case class Wins[P](p: P) extends Outcome[P]

case object Undecided extends Outcome[Nothing]

case object Draw extends Outcome[Nothing]