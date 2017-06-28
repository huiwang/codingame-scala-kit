package com.truelaurel.samplegames.wondev.domain

import com.truelaurel.algorithm.game.GameState
import com.truelaurel.collection.IterableUtil._
import com.truelaurel.math.geometry.Direction
import com.truelaurel.math.geometry.grid.FastGrid
import com.truelaurel.samplegames.wondev.domain.FastState._

case class PlayerState(units: Vector[Int],
                       grid: FastGrid,
                       score: Int = 0,
                       inactive: Boolean = false) {


  def push(unitId: Int, tgt: Direction, push: Direction): PlayerState = {
    val unitPos = units(unitId)
    val tgtPos = grid.neigborIn(unitPos, tgt)
    val tgtId = if (units(0) == tgtPos) 0 else 1
    val pushed = grid.neigborIn(tgtPos, push)
    copy(units = units.updated(tgtId, pushed))
  }

  def move(unitId: Int, direction: Direction, heights: Vector[Int]): PlayerState = {
    val unitPos = units(unitId)
    val nextPos = grid.neigborIn(unitPos, direction)
    val s = if (heights(nextPos) == SCORE_HEIGHT) score + 1 else score
    copy(
      units = units.updated(unitId, nextPos),
      score = s)
  }

}

case class FastState(size: Int,
                     turn: Int,
                     states: Seq[PlayerState],
                     myUnits: Vector[Int],
                     opUnits: Vector[Int],
                     heights: Vector[Int],
                     grid: FastGrid,
                     nextPlayer: Boolean = true,
                     myInactive: Boolean = false,
                     opInactive: Boolean = false) extends GameState[Boolean] {

  // myUnits : when nextPlayer == true

  def myScore: Int = states(0).score
  def opScore: Int = states(1).score


  def applyAction(action: WondevAction): FastState = action match {
    case MoveBuild(unitIndex, moveDir, buildDir) =>
      moveAndBuild(unitIndex, moveDir, buildDir)

    case MovePush(unitIndex, moveDir, pushDir) =>
      pushAndBuild(unitIndex, moveDir, pushDir)

    case Pass =>
      val deactivated = if (nextPlayer) copy(myInactive = true) else copy(opInactive = true)
      deactivated.endTurn
  }

  def validActions: Seq[WondevAction] = {
    val actions = for {
      id <- 0 to 1
      move <- validActionsForUnit(id)
    } yield move
    if (actions.nonEmpty)
      actions
    else Vector(Pass)
  }

  private def updatePlayerState(mine: Boolean, f: PlayerState => PlayerState) = {
    val i = if (mine) 0 else 1
    copy(states = states.updated(i, f(states(i))))
  }


  private def validActionsForUnit(id: Int): Iterable[WondevAction] =
    pushActions(id) ++ moveActions(id)

  private def pushAndBuild(unitIndex: Int, moveDir: Direction, pushDir: Direction) = {
    val unitPos = if (nextPlayer) myUnits(unitIndex) else opUnits(unitIndex)
    val tgtPos = grid.neigborIn(unitPos, moveDir)
    val pushed = updatePlayerState(!nextPlayer, _.push(unitIndex, moveDir, pushDir))
    pushed.push(unitIndex, moveDir, pushDir)
      .build(tgtPos)
      .endTurn
  }

  private def moveAndBuild(unitIndex: Int, moveDir: Direction, buildDir: Direction) = {
    val unitPos = if (nextPlayer) myUnits(unitIndex) else opUnits(unitIndex)
    val movedPos = grid.neigborIn(unitPos, moveDir)
    val builtPos = grid.neigborIn(movedPos, buildDir)
    val moved = updatePlayerState(nextPlayer, _.move(unitIndex, moveDir, heights))

    moved.move(unitIndex, moveDir)
      .build(builtPos)
      .endTurn
  }

  def endTurn: FastState =
    copy(
      nextPlayer = !nextPlayer,
      turn = turn + 1)

  def build(p: Int): FastState =
    copy(heights = heights.updatef(p, 1.+))

  //TODO : test System.arraycopy for perf

  def move(unitId: Int, direction: Direction): FastState =
    if (nextPlayer) {
      val unitPos = myUnits(unitId)
      val nextPos = grid.neigborIn(unitPos, direction)
      val units = myUnits.updated(unitId, nextPos)
      copy(myUnits = units)
    } else {
      val unitPos = opUnits(unitId)
      val nextPos = grid.neigborIn(unitPos, direction)
      val units = opUnits.updated(unitId, nextPos)
      copy(opUnits = units)
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
  def apply(size: Int, myUnits: Vector[Int], opUnits: Vector[Int]): FastState = {
    val fastGrid = FastGrid(size)
    FastState(size,
      turn = 0,
      states = Seq(
        PlayerState(myUnits, fastGrid),
        PlayerState(opUnits, fastGrid)),
      myUnits,
      opUnits,
      heights = Vector.fill(size * size)(0),
      grid = fastGrid)
  }

  val MAX_BUILT_HEIGHT = 4
  val HOLE_HEIGHT = -1
  val SCORE_HEIGHT = 3
}
