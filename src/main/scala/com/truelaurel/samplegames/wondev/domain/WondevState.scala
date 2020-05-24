package com.truelaurel.samplegames.wondev.domain

import com.truelaurel.algorithm.game.GameState
import com.truelaurel.math.geometry.Pos

case class WondevState(
    size: Int,
    heightMap: Map[Pos, Int],
    units: Seq[Pos],
    legalActions: Seq[WondevAction],
    nextPlayer: Boolean = true
) extends GameState[Boolean] {}
