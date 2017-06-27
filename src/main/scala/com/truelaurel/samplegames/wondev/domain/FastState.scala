package com.truelaurel.samplegames.wondev.domain

import com.truelaurel.collection.IterableUtil._
import com.truelaurel.math.geometry.Direction
import com.truelaurel.math.geometry.grid.FastGrid

case class FastState(size: Int,
                     turn: Int,
                     myScore: Int,
                     opScore: Int,
                     myUnits: Vector[Int],
                     opUnits: Vector[Int],
                     heights: Vector[Int],
                     grid: FastGrid,
                     nextPlayer: Boolean = true) {

  import FastState._

  def applyAction(action: WondevAction) = action match {
    case MoveBuild(unitIndex, moveDir, buildDir) =>
      moveAndBuild(unitIndex, moveDir, buildDir)

    case MovePush(unitIndex, moveDir, pushDir) =>
      pushAndBuild(unitIndex, moveDir, pushDir)

    case Pass => endTurn
  }

  private def pushAndBuild(unitIndex: Int, moveDir: Direction, pushDir: Direction) = {
    val unitPos = if (nextPlayer) myUnits(unitIndex) else opUnits(unitIndex)
    val tgtPos = grid.neigborIn(unitPos, moveDir)
    push(unitIndex, moveDir, pushDir)
      .build(tgtPos)
      .endTurn
  }

  private def moveAndBuild(unitIndex: Int, moveDir: Direction, buildDir: Direction) = {
    val unitPos = if (nextPlayer) myUnits(unitIndex) else opUnits(unitIndex)
    val movedPos = grid.neigborIn(unitPos, moveDir)
    val builtPos = grid.neigborIn(movedPos, buildDir)
    move(unitIndex, moveDir)
      .build(builtPos)
      .endTurn
  }

  def endTurn: FastState =
    copy(nextPlayer = !nextPlayer)

  def build(p: Int): FastState =
    copy(heights = heights.updatef(p, 1 +))

  def move(unitId: Int, direction: Direction): FastState =
    if (nextPlayer) {
      val unitPos = myUnits(unitId)
      val nextPos = grid.neigborIn(unitPos, direction)
      val score = if (heights(nextPos) == SCORE_HEIGHT) myScore + 1 else myScore
      val units = myUnits.updated(unitId, nextPos)
      copy(myUnits = units, myScore = score)
    } else {
      val unitPos = opUnits(unitId)
      val nextPos = grid.neigborIn(unitPos, direction)
      val score = if (heights(nextPos) == SCORE_HEIGHT) opScore + 1 else opScore
      val units = opUnits.updated(unitId, nextPos)
      copy(opUnits = units, opScore = score)
    }

  def push(unitId: Int, tgt: Direction, push: Direction): FastState =
    if (nextPlayer) {
      val unitPos = myUnits(unitId)
      val tgtPos = grid.neigborIn(unitPos, tgt)
      val tgtId = if (opUnits(0) == tgtPos) 0 else 1
      val pushed = grid.neigborIn(tgtPos, push)
      val units = opUnits.updated(tgtId, pushed)
      copy(opUnits = units)
    } else {
      val unitPos = opUnits(unitId)
      val tgtPos = grid.neigborIn(unitPos, tgt)
      val tgtId = if (myUnits(0) == tgtPos) 0 else 1
      val pushed = grid.neigborIn(tgtPos, push)
      val units = myUnits.updated(tgtId, pushed)
      copy(myUnits = units)
    }
}

object FastState {
  def apply(size: Int, myUnits: Vector[Int], opUnits: Vector[Int]): FastState =
    FastState(size,
      turn = 0,
      myScore = 0,
      opScore = 0,
      myUnits,
      opUnits,
      heights = Vector.fill(size * size)(0),
      grid = FastGrid(size))

  val MAX_BUILT_HEIGHT = 4
  val SCORE_HEIGHT = 3
}
