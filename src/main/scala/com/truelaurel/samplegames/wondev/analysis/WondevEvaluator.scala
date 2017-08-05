package com.truelaurel.samplegames.wondev.analysis

import java.util

import com.truelaurel.math.geometry.{Direction, Pos}
import com.truelaurel.samplegames.wondev.domain._

/**
  * Created by hwang on 24/06/2017.
  */
object WondevEvaluator {

  def evaluate(state: MutableWondevState): Double = {
    evaluateUnits(state, state.readable.myUnits) - 2.0 * evaluateUnits(state, state.readable.opUnits.filter(WondevContext.isVisible))
  }


  private def evaluateUnits(state: MutableWondevState, units: Seq[Pos]): Double = {
    units.map(unit => {
      countExits(state, unit, 3) + state.readable.heightOf(unit)
    }).sum
  }

  private def countExits(state: MutableWondevState, unit: Pos, depth: Int): Double = {
    val queue = new util.LinkedList[Pos]()
    var reached = 0.0
    queue.add(unit)
    while (!queue.isEmpty) {
      val next = queue.remove()
      val neighbors = state.readable.neighborOf(next)
      val reachableHeight = state.readable.heightOf(next) + 1
      val distance1 = unit.distance(next)
      if (distance1 != 0) {
        reached += 1.0 / distance1
      }
      neighbors
        .foreach(neighbor => {
          val distance2 = unit.distance(neighbor)
          val neighborHeight = state.readable.heightOf(neighbor)
          if (distance2 > distance1 &&
            distance2 <= depth &&
            WondevContext.isPlayable(neighborHeight) &&
            neighborHeight <= reachableHeight &&
            state.readable.isFree(neighbor)
          ) {
            queue.add(neighbor)
          }
        })
    }
    reached
  }

}
