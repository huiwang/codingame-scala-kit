package com.truelaurel.samplegames.wondev.domain

import com.truelaurel.math.geometry.Pos

case class WondevContext(size: Int, unitsperplayer: Int)

case class WondevState(context: WondevContext,
                       turn: Int = 0,
                       heights: Map[Pos, Int] = Map.empty,
                       myUnits: Seq[Pos] = Nil,
                       opUnits: Seq[Pos] = Nil,
                       legalActions: Seq[WondevAction] = Nil)

