package com.truelaurel.math.geometry

case class Pos(x: Int, y: Int) {
  def neighbours4: Seq[Pos] =
    Seq(Pos(x + 1, y),
      Pos(x - 1, y),
      Pos(x, y - 1),
      Pos(x, y + 1))
}

object Pos {
  val right: (Int, Int) = (1, 0)
  val down: (Int, Int) = (0, 1)
  val downRight: (Int, Int) = (1, 1)
  val downLeft: (Int, Int) = (-1, 1)
  val all = Seq(right, down, downRight, downLeft)
}
