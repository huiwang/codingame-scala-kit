package com.truelaurel.samplegames.wondev.analysis

import com.truelaurel.math.geometry.{Directions, Pos}
import com.truelaurel.samplegames.wondev.domain._

/**
  * Created by hwang on 24/06/2017.
  */
object WondevAnalysis {
  def neighborsOf(pos: Pos, size: Int): Seq[Pos] = {
    Directions.all
      .map(pos.+)
      .filter(p => p.x < size && p.x > 0 && p.y < size && p.y > 0)
  }

  def evaluate(state: WondevState): Double = {
    val myAccesible = state.myUnits.map(countAccessibleNeighbors(state, _)).sum
    val opAccesible = state.opUnits.filter(_.x != -1).map(countAccessibleNeighbors(state, _)).sum
    myAccesible - opAccesible
  }

  private def countAccessibleNeighbors(state: WondevState, unit: Pos) = {
    val neighbors = neighborsOf(unit, state.context.size)
    val myHeight = heightOf(unit, state)
    val accessible = neighbors.map(heightOf(_, state)).filter(h => h != 4 && h != -1).count(h => h <= myHeight + 1)
    accessible
  }

  def heightOf(pos: Pos, state: WondevState): Int = {
    state.heights(pos)
  }
}
