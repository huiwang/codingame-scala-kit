package com.truelaurel.samplegames.wondev.domain

import com.truelaurel.math.geometry.Pos

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
  val positions: Array[Int] = (for {
    x <- 0 until size
    y <- 0 until size
  } yield x + y * size).toArray

  def pos(p: Int): Pos = {
    val x = p % size
    val y = p / size
    Pos(x, y)
  }
}