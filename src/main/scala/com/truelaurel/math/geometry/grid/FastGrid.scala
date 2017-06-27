package com.truelaurel.math.geometry.grid

import com.truelaurel.math.geometry._

/**
  * Square grid where positions are represented by numbers.
  * Provides fast access to neighbors.
  *
  * @param size the length of one side of the square
  */
case class FastGrid(size: Int) {
  def neigborIn(pos: Int, d: Direction): Int = d match {
    case N => pos - size
    case W => pos - 1
    case S => pos + size
    case E => pos + 1
    case NW => pos - size - 1
    case NE => pos - size + 1
    case SW => pos + size - 1
    case SE => pos + size + 1
  }

  val center: Int = pos(Pos(size / 2, size / 2))

  val neighbors: Array[Array[Int]] = for {
    p <- (0 until size * size).toArray
  } yield Direction.neighborsOf(pos(p), size).map(pos).toArray


  def pos(p: Pos): Int =
    p.x + p.y * size


  def pos(p: Int): Pos = {
    val x = p % size
    val y = p / size
    Pos(x, y)
  }
}

