package com.truelaurel.samplegames.wondev.domain

import com.truelaurel.math.geometry._


case class WondevState(context: WondevContext,
                       turn: Int,
                       heightMap: Map[Pos, Int],
                       myUnits: Seq[Pos],
                       opUnits: Seq[Pos],
                       legalActions: Seq[LegalAction] = Nil) {

  def neighborMap: Map[Pos, Set[Pos]] = WondevContext.neighborMapBySize(context.size)
}

case class FastState(turn: Int,
                     myScore: Int,
                     myUnit: Array[Int],
                     opUnits: Array[Int],
                     heights: Array[Int],
                     size: Int) {


}

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