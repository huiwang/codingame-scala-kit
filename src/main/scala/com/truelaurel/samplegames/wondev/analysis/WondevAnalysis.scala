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
  }

  def evaluate(state: WondevState, oppoPos: Option[Pos]): Double = {
    val myAccesible = unitsAccessibles(state, state.myUnits)
    if (state.opUnits.forall(_.x == -1)) {
      val opAccesible = oppoPos.map(pos => {
        unitsAccessibles(state, Vector(pos))
      }).getOrElse(0.0)
      myAccesible - opAccesible
    } else {
      val opAccesible = unitsAccessibles(state, state.opUnits.filter(_.x != -1))
      myAccesible - opAccesible
    }
  }

  private def unitsAccessibles(state: WondevState, myUnits: Seq[Pos]): Double = {
    myUnits.map(countAccessibleNeighbors(state, state.heightMap, _, state.context.size / 2)).sum
  }

  def findDelta(state: WondevState): Option[Pos] = {
    state.heightMap.find { case (pos, height) =>
      state.context.previousHeightMap.get(pos).exists(h => height - h == 1)
    }.map(p => p._1)
  }

  private def countAccessibleNeighbors(state: WondevState, heightMap: Map[Pos, Int], unit: Pos, depth: Int) = {
    val queue = new util.LinkedList[Pos]()
    var reached = 0
    queue.add(unit)
    var neighborMap = WondevContext.neighborMapBySize(state.context.size)
    while (!queue.isEmpty) {
      val next = queue.remove()
      reached += 1
      val neighbors = neighborMap(next)
      val height = heightMap(next)
      val distance1 = unit.distance(next)
      neighbors
        .foreach(neighbor => {
          val distance2 = unit.distance(neighbor)
          val neighborHeight = state.heightMap(neighbor)
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
