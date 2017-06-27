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



