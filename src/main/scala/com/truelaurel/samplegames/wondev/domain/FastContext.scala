package com.truelaurel.samplegames.wondev.domain

import com.truelaurel.math.geometry.Pos
import com.truelaurel.math.geometry.grid.FastGrid

case class FastContext(size: Int, unitsperplayer: Int, stateAfterMyAction: Option[FastState] = None) {

  val center = Pos(size / 2, size / 2)

  def distToCenter(p: Pos): Int = (p.x - center.x).abs + (p.y - center.y).abs

  def findHeightDelta(previousHeights: Array[Int]): Option[Int] =
    stateAfterMyAction.flatMap { state =>
      (0 until size * size)
        .find { i => state.heights(i) != previousHeights(i) }
    }

  def guessOppPos(heights: Array[Int]): Option[Array[Int]] = stateAfterMyAction.map { state =>
    val delta = findHeightDelta(heights)
    val possibilities = for {
      p <- delta.toArray
      n <- state.grid.neighbors(p)
      if state.validUnitPos(p) && !state.opUnits.contains(p)
    } yield p
    possibilities.sortBy(state.heights)
  }
}

import com.truelaurel.samplegames.wondev.domain.FastState._

object FastContext {
  def notSeenByMine(mine: Array[Int], grid: FastGrid, heights: Array[Int]): Array[Int] = {
    val seen = (mine ++ mine.flatMap(m => grid.neighbors(m))).distinct
    for {
      p <- heights
      if grid.isValid(p) &&
        heights(p) < MAX_BUILT_HEIGHT &&
        heights(p) != HOLE_HEIGHT &&
        !seen.contains(p)
    } yield p
  }
}
