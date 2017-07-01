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
    }.toArray
    stateAfterMyAction match {
      case None => guessWithoutLastState(heights, mine, opponents)
      case Some(state) => guessFromLastState(heights, mine, state, opponents)
    }
  }

  private def guessWithoutLastState(heights: Array[Int], mine: Array[Int], opponents: Array[Int]) =
    opponents.map {
      case -1 => FastContext.notSeenByMine(mine, grid, heights) diff opponents
      case p => Array(p)
    }

  private def guessFromLastState(heights: Array[Int],
                                 mine: Array[Int],
                                 state: FastState,
                                 opponents: Array[Int]): Array[Array[Int]] = {
    val guessesAssumingUnitBuilt = opponents.zipWithIndex.map {
      case (-1, i) =>
        if (unitsperplayer > 1) {
          val other = opponents(1 - i)
          val otherMoved = unitsperplayer > 1 && !state.possibleOpUnits(1 - i).contains(other)
          if (otherMoved) state.possibleOpUnits(i)
          else guessOnePos(heights, mine, state, other, i)
        } else guessOnePos(heights, mine, state, -1, i)
      case (p, _) => Array(p)
    }
    val hasSurePosition = guessesAssumingUnitBuilt.zipWithIndex.collect {
      case (guess, i) if guess.size == 1 => i
    }.headOption

    guessesAssumingUnitBuilt
  }

  private def guessOnePos(heights: Array[Int], mine: Array[Int], state: FastState, other: Int, i: Int): Array[Int] = {
    val inFog = FastContext.notSeenByMine(mine, state.grid, heights)
    (findHeightDelta(heights) match {
      case Some(delta) => guessPosWithDelta(state, i, delta)
      case None => state.possibleOpUnits(i)
    }) intersect inFog diff Array(other)
  }

  private def guessPosWithDelta(state: FastState, i: Int, delta: Int) = {
    val possibleWithDelta = state.grid.neighbors(delta)
    //TODO : check height
    val reachableFromLastTurn = state.possibleOpUnits(i).flatMap { p =>
      state.grid.neighbors(p)
    }.distinct
    (possibleWithDelta intersect reachableFromLastTurn).distinct
  }
}

object FastContext {
  def notSeenByMine(mine: Array[Int], grid: FastGrid, heights: Array[Int]): Array[Int] = {
    val seen = (mine ++ mine.flatMap(m => grid.neighbors(m))).distinct
    val accessible = FastState.accessible(heights)
    accessible diff seen
  }
}
