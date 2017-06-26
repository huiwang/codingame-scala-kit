package com.truelaurel.samplegames.wondev.domain

import com.truelaurel.math.geometry.{Direction, Pos}
import com.truelaurel.samplegames.wondev.analysis.WondevAnalysis


case class WondevContext(size: Int, unitsperplayer: Int, previousHeightMap: Map[Pos, Int] = Map.empty) {

  def neighborsMap: Map[Pos, Set[Pos]] = WondevContext.neighborMapBySize(size)

  val center = Pos(size / 2, size / 2)

  def distToCenter(p: Pos): Int = (p.x - center.x).abs + (p.y - center.y).abs
}


object WondevContext {

  val neighborMapBySize: Map[Int, Map[Pos, Set[Pos]]] = Map(
    5 -> computeNeighborMap(5),
    6 -> computeNeighborMap(6),
    7 -> computeNeighborMap(7)
  )

  def computeNeighborMap(size: Int): Map[Pos, Set[Pos]] = (for {
    x <- 0 until size
    y <- 0 until size
    pos = Pos(x, y)
    neighbors = WondevAnalysis.neighborsOf(pos, size)
  } yield pos -> neighbors).toMap
}

sealed trait LegalActionType

case object Build extends LegalActionType

case object Push extends LegalActionType

case class LegalAction(actionType: LegalActionType, unitIndex: Int, dir1: Direction, dir2: Direction)

case class WondevState(context: WondevContext,
                       turn: Int,
                       heightMap: Map[Pos, Int],
                       myUnits: Seq[Pos],
                       opUnits: Seq[Pos],
                       legalActions: Seq[LegalAction]) {

  def neighborMap: Map[Pos, Set[Pos]] = WondevContext.neighborMapBySize(context.size)
}
