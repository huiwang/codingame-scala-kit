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
  def neighborIn(pos: Int, d: Direction): Int = d match {
    case N => pos - size
    case W => pos - 1
    case S => pos + size
    case E => pos + 1
    case NW => pos - size - 1
    case NE => pos - size + 1
    case SW => pos + size - 1
    case SE => pos + size + 1
  }

  def checkedNeighborIn(p: Int, d: Direction): Option[Int] = {
    val po = pos(p)
    val canN = po.y > 0
    val canS = po.y < size - 1
    val canE = po.x < size - 1
    val canW = po.x > 0
    d match {
      case N if canN => Some(p - size)
      case W if canW => Some(p - 1)
      case S if canS => Some(p + size)
      case E if canE => Some(p + 1)
      case NW if canN && canW => Some(p - size - 1)
      case NE if canE && canN => Some(p - size + 1)
      case SW if canW && canS => Some(p + size - 1)
      case SE if canE && canS => Some(p + size + 1)
      case _ => None
    }
  }


  val center: Int = pos(Pos(size / 2, size / 2))

  val size2: Int = size * size
  /**
    * Only valid neighbors are listed here.
    */
  val namedNeighbors: Array[Array[(Direction, Int)]] = for {
    p <- (0 until size2).toArray
    po = pos(p)
  } yield namedNeighborsImpl(po)

  val neighbors: Array[Array[Int]] = namedNeighbors.map(a => a.map(_._2))

  private def namedNeighborsImpl(p: Pos) = {
    for {
      d <- Direction.all.toArray
      r = p.neighborIn(d)
      if isValid(r)
    } yield (d: Direction) -> pos(r)
  }

  def pos(p: Pos): Int =
    p.x + p.y * size


  def pos(p: Int): Pos = {
    val x = p % size
    val y = p / size
    Pos(x, y)
  }

  def isValid(p: Int): Boolean = p >= 0 && p < size2

  def isValid(pos: Pos): Boolean = pos.x < size && pos.x >= 0 && pos.y < size && pos.y >= 0
}

