package com.truelaurel.samplegames.wondev.domain

import com.truelaurel.math.geometry.Pos
import com.truelaurel.samplegames.wondev.analysis.WondevAnalysis


case class WondevContext(size: Int,
                         players: Int) {}

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
      pushable = neighborNeighbor.filter(nn => nn.distanceEuclide(pos) > 2.0 || (nn.distance(pos) == 2 && pos.distanceEuclide(neighbor) == 1.0))
    } yield (pos, neighbor) -> pushable).toMap
    map
  }

  def isPlayable(height: Int): Boolean = height > -1 && height < 4
}


case class WondevState(size: Int,
                       heightMap: Map[Pos, Int],
                       units: Seq[Pos],
                       legalActions: Seq[WondevAction],
                       nextPlayer: Boolean = true
                      ) {

}
