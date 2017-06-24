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
  val right: (Int, Int) = (1, 0)
  val down: (Int, Int) = (0, 1)
  val downRight: (Int, Int) = (1, 1)
  val downLeft: (Int, Int) = (-1, 1)
  val all = Seq(right, down, downRight, downLeft)
}

object Directions {
  val N = new Pos(0, -1) {
    override val toString = "N"
  }
  val S = new Pos(0, 1) {
    override val toString = "S"
  }
  val E = new Pos(1, 0) {
    override val toString = "E"
  }
  val W = new Pos(-1, 0) {
    override val toString = "W"
  }
  val NE = new Pos(1, -1) {
    override val toString = "NE"
  }
  val SE = new Pos(1, 1) {
    override val toString = "SE"
  }
  val NW = new Pos(-1, -1) {
    override val toString = "NW"
  }
  val SW = new Pos(-1, 1) {
    override val toString = "SW"
  }

  val directions = Map(
    "N" -> N,
    "S" -> S,
    "NE" -> NE,
    "SE" -> SE,
    "NW" -> NW,
    "SW" -> SW,
    "E" -> E,
    "W" -> W)

  def apply(name: String): Pos = directions(name)

  val all: Seq[Pos] = directions.values.toSeq
}