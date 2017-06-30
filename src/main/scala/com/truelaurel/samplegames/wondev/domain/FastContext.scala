package com.truelaurel.samplegames.wondev.domain

import com.truelaurel.math.geometry.Pos
import com.truelaurel.math.geometry.grid.FastGrid

case class FastContext(size: Int, unitsperplayer: Int, stateAfterMyAction: Option[FastState] = None) {
  val grid = FastGrid(size)
  val center = Pos(size / 2, size / 2)

  def distToCenter(p: Pos): Int = (p.x - center.x).abs + (p.y - center.y).abs

  def findHeightDelta(previousHeights: Array[Int]): Option[Int] =
    stateAfterMyAction.flatMap { state =>
      (0 until size * size)
        .find { i => state.heights(i) != previousHeights(i) }
    }

  def guessOppPos(heights: Array[Int], mine: Array[Int], opUnits: Seq[Pos]): Array[Array[Int]] = {
    val opponents = opUnits.map {
      case Pos(-1, -1) => -1
      case p => grid.pos(p)
    }
    val knownOpponents = opponents.filter(_ != -1)
    val possibleOppPos = (stateAfterMyAction match {
      case None => FastContext.notSeenByMine(mine, grid, heights)
      case Some(state) => guessFromLastState(heights, mine, state)
    }) diff knownOpponents
    opponents.map {
      case -1 => possibleOppPos
      case p => Array(p)
    }.toArray
  }

  private def guessFromLastState(heights: Array[Int], mine: Array[Int], state: FastState): Array[Int] = {
    val delta = findHeightDelta(heights)
    val inFog = FastContext.notSeenByMine(mine, state.grid, heights)
    val reachableFromLastTurn = state.possibleOpUnits.flatten.distinct.flatMap { p =>
      state.grid.neighbors(p)
    }.distinct
    val possibilities = for {
      p <- delta.toSeq
      n <- state.grid.neighbors(p)
      if state.validUnitPos(n) && inFog.contains(n) && reachableFromLastTurn.contains(n)
    } yield n
    //    val previousPositions = state.opUnits.filter(inFog.contains)
    //    (previousPositions ++ possibilities).distinct.sortBy(state.heights)
    possibilities.distinct.toArray
  }
}

import com.truelaurel.samplegames.wondev.domain.FastState._

object FastContext {
  def notSeenByMine(mine: Array[Int], grid: FastGrid, heights: Array[Int]): Array[Int] = {
    val seen = (mine ++ mine.flatMap(m => grid.neighbors(m))).distinct
    for {
      p <- (0 until grid.size2).toArray
      if heights(p) < MAX_BUILT_HEIGHT &&
        heights(p) != HOLE_HEIGHT &&
        !seen.contains(p)
    } yield p
  }
}
