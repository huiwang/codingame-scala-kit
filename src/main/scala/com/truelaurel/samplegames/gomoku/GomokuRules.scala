package com.truelaurel.samplegames.gomoku

import com.truelaurel.algorithm.game._
import com.truelaurel.collection.{BitGrid, GridData, Masks, Pos}

import scala.math.abs

case class GomokuRules(size: Int, lengthToWin: Int)
  extends RulesFor2p[GomokuBoard, Pos] {

  val initial: GomokuBoard = GomokuBoard(size)
  val masks = Masks(size, lengthToWin)

  def validMoves(state: GomokuBoard): Seq[Pos] = state.free.toSeq

  def applyMove(state: GomokuBoard, move: Pos): GomokuBoard =
    state.play(move)

  def outcome(b: GomokuBoard) =
    if (hasWon(b, true)) Wins(true)
    else if (hasWon(b, false)) Wins(false)
    else if (b.free.isEmpty) Draw
    else Undecided

  def hasWon(b: GomokuBoard, player: Boolean) = {
    val data = if (player) b.dataTrue else b.dataFalse
    BitGrid(data, masks).complete
  }

  def centerHeuristic(state: GomokuBoard): Double = {
    def score(data: GridData) =
      data.usedPos.map {
        case Pos(x, y) => 1.0 / (1 + abs(x - size / 2) + abs(y - size / 2))
      }.sum

    val opp = score(state.dataLast)
    val mine = score(state.dataNext)
    mine - opp
  }
}