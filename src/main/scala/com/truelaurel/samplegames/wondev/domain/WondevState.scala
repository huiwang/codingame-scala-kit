package com.truelaurel.samplegames.wondev.domain

import com.truelaurel.math.geometry.{Direction, Pos}


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