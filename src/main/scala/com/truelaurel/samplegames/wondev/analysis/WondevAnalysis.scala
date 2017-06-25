package com.truelaurel.samplegames.wondev.analysis

import java.util

import com.truelaurel.math.geometry.{Direction, Pos}
import com.truelaurel.samplegames.wondev.domain._

/**
  * Created by hwang on 24/06/2017.
  */
object WondevAnalysis {
  def neighborsOf(pos: Pos, size: Int): Seq[Pos] = {
    Direction.all
      .map(d => pos.neighborIn(d))
      .filter(p => p.x < size && p.x >= 0 && p.y < size && p.y >= 0)
  }

  def evaluate(state: WondevState): Double = {
    val myAccesible = state.myUnits.map(countAccessibleNeighbors(state, state.heights, _, state.context.size / 2)).sum
    val opAccesible = state.opUnits.filter(_.x != -1).map(countAccessibleNeighbors(state, state.heights, _, state.context.size / 2)).sum
    myAccesible - opAccesible
  }

  private def countAccessibleNeighbors(state: WondevState, heightMap: Map[Pos, Int], unit: Pos, depth: Int) = {
    val queue = new util.LinkedList[Pos]()
    var reached = 0
    queue.add(unit)
    while (!queue.isEmpty) {
      val next = queue.remove()
      reached += 1
      val neighbors = state.context.neighborsMap(next)
      val height = heightMap(next)
      val distance1 = unit.distance(next)
      neighbors
        .foreach(neighbor => {
          val distance2 = unit.distance(neighbor)
          val neighborHeight = state.heights(neighbor)
          if (distance2 > distance1 &&
            distance2 <= depth &&
            neighborHeight != 4 && neighborHeight != -1 &&
            neighborHeight <= height + 1 &&
            !state.myUnits.contains(neighbor) &&
            !state.opUnits.contains(neighbor)
          ) {
            queue.add(neighbor)
          }
        })
    }
    reached
  }

}
