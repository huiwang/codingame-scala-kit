package com.truelaurel.samplegames.stones

import com.truelaurel.algorithm.game._
import com.truelaurel.collection.{BitGrid, Masks, Pos}
import com.truelaurel.samplegames.gomoku.GomokuBoard

//Terni Lapilli game (3 stones)
object Rules extends RulesFor2p[GomokuBoard, Move] {

  val initial: GomokuBoard = GomokuBoard(3)

  val center = Pos(1, 1)
  val masks = Masks(3, 3)

  def validMoves(state: GomokuBoard): Seq[Move] = {
    val placed = if (state.nextPlayer) state.dataTrue else state.dataFalse
    val free = state.free.toSeq
    if (placed.used.size < 3) free.map(Add)
    else for {
      from <- placed.usedPos.toSeq
      diags = if (from == center) free else Seq(center).intersect(free)
      to <- (placed.neighbours4(from) ++ diags).distinct
      if state.isFree(to)
    } yield Slide(from, to)
  }

  def applyMove(state: GomokuBoard, move: Move): GomokuBoard = move match {
    case Add(p) => state.play(p)
    case Slide(from, to) => state.remove(from).play(to)
  }

  def outcome(b: GomokuBoard) =
    if (hasWon(b, true)) Wins(true)
    else if (hasWon(b, false)) Wins(false)
    else if (b.free.isEmpty) Draw
    else Undecided

  def hasWon(b: GomokuBoard, player: Boolean) = {
    val data = if (player) b.dataTrue else b.dataFalse
    BitGrid(data, masks).complete
  }
}