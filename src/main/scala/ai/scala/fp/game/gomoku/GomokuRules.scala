package ai.scala.fp.game.gomoku

import ai.scala.fp.game._
import ai.scala.fp.geo.{BitGrid, Masks, Pos}

case class GomokuRules(size: Int, lengthToWin: Int)
  extends RulesFor2p[GomokuBoard, Pos] {

  val initial: GomokuBoard = GomokuBoard(size)

  def validMoves(state: GomokuBoard): Seq[Pos] = state.free.toSeq

  def applyMove(state: GomokuBoard, move: Pos): GomokuBoard =
    state.play(move)

  def outcome(b: GomokuBoard) =
    if (hasWon(b, true)) Wins(true)
    else if (hasWon(b, false)) Wins(false)
    else if (b.free.isEmpty) Draw
    else Undecided

  val masks = Masks(size, lengthToWin)

  def hasWon(b: GomokuBoard, player: Boolean) = {
    val data = if (player) b.dataTrue else b.dataFalse
    BitGrid(data, masks).complete
  }
}