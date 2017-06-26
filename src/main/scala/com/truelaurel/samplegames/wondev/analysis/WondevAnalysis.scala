package com.truelaurel.samplegames.wondev.analysis

import com.truelaurel.math.geometry.Pos
import com.truelaurel.samplegames.wondev.domain._

/**
  * Created by hwang on 24/06/2017.
  */
object WondevAnalysis {

  def evaluate(unit: Pos, state: WondevState): Int =
    if (unit == Pos(-1, -1)) 0
    else {
      val neighbors: Set[Pos] = state.context.neighborsMap(unit)
      val height = state.heightMap(unit)
      val free = neighbors
        .map(state.heightMap)
        .count(h => h >= 0 && h < 4 && h <= height + 1)
      free + state.context.distToCenter(unit)
    }

  def evaluate(state: WondevState, oppoPos: Option[Pos]): Double = {
    val myScore = state.myUnits.map(u => evaluate(u, state)).sum
    val opScore = state.opUnits.map(u => evaluate(u, state)).sum
    myScore - opScore
  }

  def findDelta(state: WondevState): Option[Pos] = {
    state.heightMap.find { case (pos, height) =>
      state.context.previousHeightMap.get(pos).exists(h => height - h == 1)
    }.map(p => p._1)
  }

}
