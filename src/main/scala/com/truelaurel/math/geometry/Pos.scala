package com.truelaurel.math.geometry

case class Pos(x: Int, y: Int) {
  def neighbours4: Seq[Pos] =
    Seq(Pos(x + 1, y),
      Pos(x - 1, y),
      Pos(x, y - 1),
      Pos(x, y + 1))

  def +(pos: Pos): Pos = Pos(x + pos.x, y + pos.y)

  def +(direction: String): Pos = this + (Directions.directions(direction))
}

object Pos {
  val right = (1, 0)
  val down = (0, 1)
  val downRight = (1, 1)
  val downLeft = (-1, 1)
  val all = Seq(right, down, downRight, downLeft)
}

object Directions {
  val N = Pos(0, -1)
  val S = Pos(0, 1)
  val E = Pos(1, 0)
  val W = Pos(-1, 0)
  val NE = N + E
  val SE = S + E
  val NW = N + W
  val SW = S + W

  val directions = Map(
    "N" -> N,
    "S" -> S,
    "NE" -> NE,
    "SE" -> SE,
    "NW" -> NW,
    "SW" -> SW,
    "E" -> E,
    "W" -> W)
}