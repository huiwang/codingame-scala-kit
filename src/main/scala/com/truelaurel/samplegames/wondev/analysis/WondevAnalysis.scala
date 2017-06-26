package com.truelaurel.samplegames.wondev.analysis

import com.truelaurel.math.geometry.{Direction, Pos}
import com.truelaurel.samplegames.wondev.domain._

/**
  * Created by hwang on 24/06/2017.
  */
object WondevAnalysis {
  def neighborsOf(pos: Pos, size: Int): Set[Pos] = {
    Direction.all
      .map(d => pos.neighborIn(d))
      .filter(p => p.x < size && p.x >= 0 && p.y < size && p.y >= 0)
  }

  def evaluate(unit: Pos, state: WondevState): Int =
    if (unit == Pos(-1, -1)) 0
    else {
      val neighbors: Set[Pos] = state.context.neighborsMap(unit)
      val height = state.heightMap(unit)
      neighbors
        .map(state.heightMap)
        .count(h => h >= 0 && h < 4 && h <= height + 1)
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
