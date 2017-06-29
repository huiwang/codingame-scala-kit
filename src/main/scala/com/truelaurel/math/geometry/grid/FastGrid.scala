package com.truelaurel.math.geometry.grid

import com.truelaurel.math.geometry._

/**
  * Square grid where positions are represented by numbers.
  * Provides fast access to neighbors.
  *
  * @param size the length of one side of the square
  */
case class FastGrid(size: Int) {
  /**
    * This may return an invalid neighbor if called in a direction outside grid !
    */
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

  /**
    * Only valid neighbors are listed here.
    */
  val neighbors: Array[Array[Int]] = for {
    p <- (0 until size * size).toArray
  } yield Direction.neighborsOf(pos(p), size).map(pos).toArray

  val namedNeighbors: Array[Array[(Direction, Int)]] = for {
    p <- (0 until size * size).toArray
    po = pos(p)
  } yield namedNeighborsImpl(po)


  private def namedNeighborsImpl(p: Pos) = {
    for {
      d <- Direction.all.toArray
      r = p.neighborIn(d)
      if r.x < size && r.x >= 0 && r.y < size && r.y >= 0
    } yield (d: Direction) -> pos(r)
  }

  def pos(p: Pos): Int =
    p.x + p.y * size


  def pos(p: Int): Pos = {
    val x = p % size
    val y = p / size
    Pos(x, y)
  }

  def isValid(p: Int): Boolean = p >= 0 && p < size * size
  def isValid(pos: Pos): Boolean =pos.x < size && pos.x >= 0 && pos.y < size && pos.y >= 0
}

