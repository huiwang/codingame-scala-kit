package com.truelaurel.samplegames.wondev.domain

import com.truelaurel.math.geometry.{Direction, Pos}
import com.truelaurel.samplegames.wondev.analysis.WondevAnalysis


case class WondevContext(size: Int, unitsperplayer: Int) {

  val neighborsMap: Map[Pos, Seq[Pos]] = (for {
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
                       heights: Map[Pos, Int],
                       myUnits: Seq[Pos],
                       opUnits: Seq[Pos],
                       legalActions: Seq[LegalAction])
