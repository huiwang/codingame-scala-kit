package com.truelaurel.samplegames.wondev.domain

import com.truelaurel.math.geometry.Pos
import com.truelaurel.samplegames.wondev.analysis.WondevAnalysis


case class WondevContext(size: Int, unitsperplayer: Int)

object WondevContext {
  private val maxGridSize = 7

  val neighborsMap: Map[Pos, Seq[Pos]] = (for {
    x <- 0 until maxGridSize
    y <- 0 until maxGridSize
    pos = Pos(x, y)
    neighbors = WondevAnalysis.neighborsOf(pos, maxGridSize)
  } yield pos -> neighbors).toMap
}

case class WondevState(context: WondevContext,
                       turn: Int,
                       heights: Map[Pos, Int],
                       myUnits: Seq[Pos],
                       opUnits: Seq[Pos],
                       legalActions: Seq[WondevAction])
