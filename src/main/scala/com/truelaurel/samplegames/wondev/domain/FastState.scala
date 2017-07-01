package com.truelaurel.samplegames.wondev.domain

import com.truelaurel.algorithm.game.GameState
import com.truelaurel.collection.ArrayUtil._
import com.truelaurel.math.geometry.grid.FastGrid
import com.truelaurel.math.geometry.{Direction, Pos}
import com.truelaurel.samplegames.wondev.domain.FastState._

import scala.collection.mutable.ListBuffer

case class PlayerState(units: Array[Int],
                       grid: FastGrid,
                       score: Int = 0,
                       inactive: Boolean = false) {


  def push(unitId: Int, push: Direction): PlayerState = {
    val unitPos = units(unitId)
    val pushed = grid.neigborIn(unitPos, push)
    copy(units = units.updated(unitId, pushed))
  }

  def move(unitId: Int, direction: Direction, heights: Array[Int]): PlayerState = {
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
                     states: Array[PlayerState],
                     heights: Array[Int],
                     grid: FastGrid,
                     possibleOpUnits: Array[Array[Int]] = Array(),
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
    val actions = generateValidActions
    if (actions.nonEmpty)
      actions
    else Vector(Pass)
  }

  private def generateValidActions: Seq[WondevAction] = {
    val actions = ListBuffer.empty[WondevAction]
    var id = 0
    while (id <= 1) {
      addValidActionsForUnit(id, actions)
      id += 1
    }
    actions
  }

  private def updatePlayerState(mine: Boolean, f: PlayerState => PlayerState) = {
    val i = if (mine) 0 else 1
    copy(states = states.updated(i, f(states(i))))
  }


  private def addValidActionsForUnit(id: Int, buffer: ListBuffer[WondevAction]) = {
    pushActions(id, buffer)
    moveActions(id, buffer)
  }

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
    copy(heights = update(heights, p, i => 1 + i))

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

  def moveActions(unitId: Int): Seq[WondevAction] = {
    val buffer = ListBuffer.empty[WondevAction]
    moveActions(unitId, buffer)
    buffer
  }

  def moveActions(unitId: Int, buffer: ListBuffer[WondevAction]): Unit = {
    val unitPos = nextPlayerState.units(unitId)
    val startHeight = heights(unitPos)
    val otherUnits = others(unitId)
    var dir = 0
    val directions = grid.namedNeighbors(unitPos)
    val size = directions.length
    while (dir < size) {
      val (moveDir, tgtPos) = directions(dir)
      if (validMoveTarget(tgtPos, otherUnits, startHeight))
        addBuildMoves(buffer, tgtPos, otherUnits, moveDir, unitId)
      dir += 1
    }
  }

  private def others(unitId: Int) = {
    if (unitId == 0) Array(nextPlayerState.units(1), otherPlayerState.units(0), otherPlayerState.units(1))
    else Array(nextPlayerState.units(0), otherPlayerState.units(0), otherPlayerState.units(1))
  }

  private def addBuildMoves(moves: ListBuffer[WondevAction], tgtPos: Int, otherUnits: Array[Int], moveDir: Direction, unitId: Int): Unit = {
    val neighbors = grid.namedNeighbors(tgtPos)
    val size = neighbors.length
    var dir = 0
    while (dir < size) {
      val (buildDir, buildPos) = neighbors(dir)
      if (validBuildTarget(buildPos, otherUnits))
        moves += MoveBuild(unitId, moveDir, buildDir)
      dir += 1
    }
  }

  private def validMoveTarget(tgtPos: Int, otherUnits: Array[Int], startHeight: Int) =
    validBuildTarget(tgtPos, otherUnits) && heights(tgtPos) <= startHeight + 1

  private def validBuildTarget(tgtPos: Int, otherUnits: Array[Int]) =
    validUnitPos(tgtPos) && fastNotContains3(otherUnits, tgtPos)

  def validUnitPos(tgtPos: Int): Boolean =
    grid.isValid(tgtPos) &&
      heights(tgtPos) < MAX_BUILT_HEIGHT &&
      heights(tgtPos) != HOLE_HEIGHT


  def pushActions(unitId: Int): Seq[WondevAction] = {
    val buffer = ListBuffer.empty[WondevAction]
    pushActions(unitId, buffer)
    buffer
  }

  def pushActions(unitId: Int, buffer: ListBuffer[WondevAction]): Unit = {
    val unitPos = nextPlayerState.units(unitId)
    val otherUnits = others(unitId)

    val opUnits = otherPlayerState.units
    val directions = grid.namedNeighbors(unitPos)
    val size = directions.length
    var dir = 0
    while (dir < size) {
      val (moveDir, tgtPos) = directions(dir)
      if (fastContains2(opUnits, tgtPos))
        addPushMoves(buffer, tgtPos, otherUnits, moveDir, unitId)
      dir += 1
    }
  }

  private def addPushMoves(moves: ListBuffer[WondevAction], tgtPos: Int, otherUnits: Array[Int], moveDir: Direction, unitId: Int) = {
    var dir = 0
    val startHeight = heights(tgtPos)
    val directions = moveDir.similar
    while (dir < 3) {
      val pushDir = directions(dir)
      val pushPos = grid.neigborIn(tgtPos, pushDir)
      if (validMoveTarget(pushPos, otherUnits, startHeight))
        moves += MovePush(unitId, moveDir, pushDir)
      dir += 1
    }
  }


  def myScore: Int = me.score

  def opScore: Int = op.score

  def myUnits: Array[Int] = me.units

  def opUnits: Array[Int] = op.units

  def myInactive: Boolean = me.inactive

  def opInactive: Boolean = op.inactive

  def playerState(mine: Boolean): PlayerState = states(if (mine) 0 else 1)

  def nextPlayerState: PlayerState = playerState(nextPlayer)

  def otherPlayerState: PlayerState = playerState(!nextPlayer)

  override def toString = {
    Seq.tabulate(grid.size)(y => lineAt(y))
  }.mkString("\n", "\n", "\n")

  private def lineAt(y: Int) = {
    Seq.tabulate(grid.size)(x => charAt(x, y)).mkString
  }

  private def charAt(x: Int, y: Int): Char = {
    val p = grid.pos(Pos(x, y))
    if (states(0).units(0) == p) 'A'
    else if (states(0).units(1) == p) 'B'
    else if (states(1).units(1) == p) 'Z'
    else if (states(1).units(0) == p) 'Y'
    else if (heights(p) == HOLE_HEIGHT) ' '
    else heights(p).toString.head
  }

}

object FastState {
  def apply(size: Int, myUnits: Array[Int], opUnits: Array[Int]): FastState = {
    val fastGrid = FastGrid(size)
    FastState(size,
      turn = 0,
      states = Array(
        PlayerState(myUnits, fastGrid),
        PlayerState(opUnits, fastGrid)),
      heights = Array.fill(size * size)(0),
      grid = fastGrid)
  }

  val MAX_BUILT_HEIGHT = 4
  val HOLE_HEIGHT = -1
  val SCORE_HEIGHT = 3

  def accessible(heights: Array[Int]): Array[Int] =
    for {
      (h, i) <- heights.zipWithIndex
      if h != MAX_BUILT_HEIGHT && h != HOLE_HEIGHT
    } yield i
}
