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


  def push(unitId: Int, push: Direction): PlayerState = {
    val unitPos = units(unitId)
    val pushed = grid.neigborIn(unitPos, push)
    copy(units = units.updated(unitId, pushed))
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
                     heights: Vector[Int],
                     grid: FastGrid,
                     nextPlayer: Boolean = true) extends GameState[Boolean] {


  val me = states(0)
  val op = states(1)

  def applyAction(action: WondevAction): FastState = action match {
    case MoveBuild(unitIndex, moveDir, buildDir) =>
      moveAndBuild(unitIndex, moveDir, buildDir)

    case MovePush(unitIndex, moveDir, pushDir) =>
      pushAndBuild(unitIndex, moveDir, pushDir)

    case Pass =>
      updatePlayerState(nextPlayer, _.copy(inactive = true)).endTurn
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
    val unitPos = nextPlayerState.units(unitIndex)
    val tgtPos = grid.neigborIn(unitPos, moveDir)
    push(unitIndex, moveDir, pushDir)
      .build(tgtPos)
      .endTurn
  }

  private def moveAndBuild(unitIndex: Int, moveDir: Direction, buildDir: Direction) = {
    val unitPos = nextPlayerState.units(unitIndex)
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
    copy(heights = heights.updatef(p, 1.+))

  //TODO : test System.arraycopy for perf

  def move(unitIndex: Int, direction: Direction): FastState = {
    updatePlayerState(nextPlayer, _.move(unitIndex, direction, heights))
  }

  def push(unitIndex: Int, tgt: Direction, push: Direction): FastState = {
    val unitPos = nextPlayerState.units(unitIndex)
    val tgtPos = grid.neigborIn(unitPos, tgt)
    val otherUnits = otherPlayerState.units
    val tgtId = if (otherUnits(0) == tgtPos) 0 else 1
    updatePlayerState(!nextPlayer, _.push(tgtId, push))
  }

  //TODO : make faster ! use grid.neighbors
  def moveActions(unitId: Int): Iterable[MoveBuild] = {
    val allUnits = myUnits ++ opUnits
    val unitPos = nextPlayerState.units(unitId)
    val startHeight = heights(unitPos)
    val otherUnits = allUnits diff Seq(unitPos)
    for {
      moveDir <- Direction.all
      tgtPos = grid.neigborIn(unitPos, moveDir)
      if validMoveTarget(tgtPos, otherUnits, startHeight)
      buildDir <- Direction.all
      buildPos = grid.neigborIn(tgtPos, buildDir)
      if validBuildTarget(buildPos, otherUnits)
    } yield MoveBuild(unitId, moveDir, buildDir)
  }

  private def validMoveTarget(tgtPos: Int, otherUnits: Vector[Int], startHeight: Int) =
    validBuildTarget(tgtPos, otherUnits) && heights(tgtPos) <= startHeight + 1

  private def validBuildTarget(tgtPos: Int, otherUnits: Vector[Int]) =
    grid.isValid(tgtPos) &&
      !otherUnits.contains(tgtPos) &&
      heights(tgtPos) < MAX_BUILT_HEIGHT &&
      heights(tgtPos) != HOLE_HEIGHT

  //TODO : make faster ! use grid.neighbors
  def pushActions(unitId: Int): Iterable[MovePush] = {
    val unitPos = nextPlayerState.units(unitId)
    val allUnits = myUnits ++ opUnits
    val otherUnits = allUnits diff Seq(unitPos)

    val others = otherPlayerState.units
    val pushable = others.filter(grid.neighbors(unitPos).contains)
    for {
      moveDir <- Direction.all
      tgtPos = grid.neigborIn(unitPos, moveDir)
      if pushable.contains(tgtPos)
      startHeight = heights(tgtPos)
      pushDir <- moveDir.similar
      pushPos = grid.neigborIn(tgtPos, pushDir)
      if validMoveTarget(pushPos, otherUnits, startHeight)
    } yield MovePush(unitId, moveDir, pushDir)
  }

  def myScore: Int = me.score

  def opScore: Int = op.score

  def myUnits: Vector[Int] = me.units

  def opUnits: Vector[Int] = op.units

  def myInactive: Boolean = me.inactive

  def opInactive: Boolean = op.inactive

  def playerState(mine: Boolean): PlayerState = states(if (mine) 0 else 1)

  def nextPlayerState: PlayerState = playerState(nextPlayer)

  def otherPlayerState: PlayerState = playerState(!nextPlayer)

}

object FastState {
  def apply(size: Int, myUnits: Vector[Int], opUnits: Vector[Int]): FastState = {
    val fastGrid = FastGrid(size)
    FastState(size,
      turn = 0,
      states = Seq(
        PlayerState(myUnits, fastGrid),
        PlayerState(opUnits, fastGrid)),
      heights = Vector.fill(size * size)(0),
      grid = fastGrid)
  }

  val MAX_BUILT_HEIGHT = 4
  val HOLE_HEIGHT = -1
  val SCORE_HEIGHT = 3
}
