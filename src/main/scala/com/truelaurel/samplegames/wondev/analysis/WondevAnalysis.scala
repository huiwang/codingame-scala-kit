package com.truelaurel.samplegames.wondev.analysis

import java.util

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
      .toSet
  }

  def isVisible(pos: Pos): Boolean = pos.x != -1


  def evaluate(state: WondevState): Double = {
    evaluateUnits(state, state.units.take(2)) - 2.0 * evaluateUnits(state, state.units.takeRight(2).filter(isVisible))
  }


  private def evaluateUnits(state: WondevState, units: Seq[Pos]): Double = {
    units.map(unit => {
      countExits(state, unit, 3) + state.heightOf(unit)
    }).sum
  }

  def countExits(state: WondevState, unit: Pos, depth: Int): Double = {
    val queue = new util.LinkedList[Pos]()
    var reached = 0.0
    queue.add(unit)
    while (!queue.isEmpty) {
      val next = queue.remove()
      val neighbors = state.neighborOf(next)
      val reachableHeight = state.heightOf(next) + 1
      val distance1 = unit.distance(next)
      if (distance1 != 0) {
        reached += 1.0 / distance1
      }
      neighbors
        .foreach(neighbor => {
          val distance2 = unit.distance(neighbor)
          val neighborHeight = state.heightOf(neighbor)
          if (distance2 > distance1 &&
            distance2 <= depth &&
            WondevContext.isPlayable(neighborHeight) &&
            neighborHeight <= reachableHeight &&
            state.isFree(neighbor)
          ) {
            queue.add(neighbor)
          }
        })
    }
    reached
  }

  def extractArrayHeight(state: WondevState): Array[Array[Int]] = {
    val heights: Array[Array[Int]] = Array.ofDim(state.size, state.size)
    state.heightMap.foreach { case (pos, h) => heights(pos.x)(pos.y) = h }
    heights
  }

}
