package com.truelaurel.samplegames.wondev.domain

import com.truelaurel.math.geometry.Pos


case class WondevContext(size: Int, unitsperplayer: Int)

case class WondevState(context: WondevContext,
                       turn: Int,
                       heights: Map[Pos, Int],
                       myUnits: Seq[Pos],
                       opUnits: Seq[Pos],
                       legalActions: Seq[WondevAction])
