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

  // myUnits : when nextPlayer == true

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
    copy(
      nextPlayer = !nextPlayer,
      turn = turn + 1)

  def build(p: Int): FastState =
    copy(heights = heights.updatef(p, 1 +))

  //TODO : test System.arraycopy for perf

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

  //TODO : make faster ! use grid.neighbors
  def moveActions(unitId: Int): Iterable[MoveBuild] = {
    val allUnits = myUnits ++ opUnits
    val unitPos = if (nextPlayer) myUnits(unitId) else opUnits(unitId)
    val startHeight = heights(unitPos)
    val otherUnits = allUnits diff Seq(unitPos)
    for {
      moveDir <- Direction.all
      tgtPos = grid.neigborIn(unitPos, moveDir)
      if grid.isValid(tgtPos)
      if !otherUnits.contains(tgtPos)
      if heights(tgtPos) <= startHeight + 1 && heights(tgtPos) < MAX_BUILT_HEIGHT && heights(tgtPos) != HOLE_HEIGHT
      buildDir <- Direction.all
      buildPos = grid.neigborIn(tgtPos, buildDir)
      if grid.isValid(buildPos)
      if !otherUnits.contains(buildPos)
      if heights(buildPos) < MAX_BUILT_HEIGHT && heights(buildPos) != HOLE_HEIGHT
    } yield MoveBuild(unitId, moveDir, buildDir)
  }

  //TODO : make faster ! use grid.neighbors
  def pushActions(unitId: Int): Iterable[MovePush] = {
    val unitPos = if (nextPlayer) myUnits(unitId) else opUnits(unitId)
    val allUnits = myUnits ++ opUnits
    val otherUnits = allUnits diff Seq(unitPos)

    val others = if (nextPlayer) opUnits else myUnits
    val pushable = others.filter(grid.neighbors(unitPos).contains)
    for {
      moveDir <- Direction.all
      tgtPos = grid.neigborIn(unitPos, moveDir)
      if pushable.contains(tgtPos)
      startHeight = heights(tgtPos)
      pushDir <- moveDir.similar
      pushPos = grid.neigborIn(tgtPos, pushDir)
      if grid.isValid(pushPos)
      if heights(tgtPos) <= startHeight + 1 && heights(tgtPos) < MAX_BUILT_HEIGHT && heights(tgtPos) != HOLE_HEIGHT
      if !otherUnits.contains(pushPos)
    } yield MovePush(unitId, moveDir, pushDir)
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
  val HOLE_HEIGHT = -1
  val SCORE_HEIGHT = 3
}
