package com.truelaurel.samplegames.wondev.domain

import com.truelaurel.algorithm.game.GameState
import com.truelaurel.math.geometry.Pos
import com.truelaurel.samplegames.wondev.analysis.WondevAnalysis


case class WondevContext(size: Int,
                         unitsperplayer: Int) {
}


object WondevContext {

  val neighborMapBySize = Map(
    5 -> computeNeighborTable(5),
    6 -> computeNeighborTable(6),
    7 -> computeNeighborTable(7)
  )

  def computeNeighborTable(size: Int): Array[Array[Array[Pos]]] = {
    val neighborsTable: Array[Array[Array[Pos]]] = Array.ofDim(size, size)

    for {
      x <- 0 until size
      y <- 0 until size
      pos = Pos(x, y)
      neighbors = WondevAnalysis.neighborsOf(pos, size).toArray
    } {
      neighborsTable(x)(y) = neighbors
    }
    neighborsTable
  }


  val pushTargets = Map(
    5 -> computePushTargets(5),
    6 -> computePushTargets(6),
    7 -> computePushTargets(7)
  )

  private def computePushTargets(size: Int) = {
    val map = (for {
      x <- 0 until size
      y <- 0 until size
      pos = Pos(x, y)
      neighbor <- neighborMapBySize(size)(x)(y)
      neighborNeighbor = neighborMapBySize(size)(neighbor.x)(neighbor.y)
      pushable = neighborNeighbor.filter(nn => nn.distanceEuclide(pos) >= 2.0)
    } yield (pos, neighbor) -> pushable).toMap
    map
  }

  def isPlayable(height: Int): Boolean = height > -1 && height < 4
}


case class WondevState(context: WondevContext,
                       heightMap: Map[Pos, Int],
                       units: Seq[Pos],
                       legalActions: Seq[WondevAction],
                       nextPlayer: Boolean = true
                      ) extends GameState[Boolean] {

  private lazy val allUnits: Array[Array[Boolean]] = extractFreeCellTable

  private def extractFreeCellTable = {

    val occupyTable: Array[Array[Boolean]] = Array.fill(context.size, context.size)(true)
    units.foreach(u => if (u.x != -1) occupyTable(u.x)(u.y) = false)
    occupyTable

  }

  private lazy val heights: Array[Array[Int]] = WondevAnalysis.extractArrayHeight(this)

  def isFree(pos: Pos): Boolean = allUnits(pos.x)(pos.y)

  def heightOf(pos: Pos): Int = heights(pos.x)(pos.y)

  private val neighborTable = WondevContext.neighborMapBySize(context.size)

  def neighborOf(pos: Pos): Array[Pos] = {
    neighborTable(pos.x)(pos.y)
  }

  lazy val pushTargets: Map[(Pos, Pos), Array[Pos]] = WondevContext.pushTargets(context.size)

}
