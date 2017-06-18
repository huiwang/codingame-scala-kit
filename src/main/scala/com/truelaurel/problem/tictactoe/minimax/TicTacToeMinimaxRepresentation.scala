package com.truelaurel.problem.tictactoe.minimax

import com.truelaurel.codingame.minimax.MinimaxRepresentation

/**
  * Created by hwang on 18/06/2017.
  */
class TicTacToeMinimaxRepresentation(val root: TicTacToeState) extends MinimaxRepresentation[TicTacToeState, TicTacToeAction] {

  private val rows = Vector(
    Vector(0, 1, 2),
    Vector(3, 4, 5),
    Vector(6, 7, 8)
  )

  private val cols = Vector(
    Vector(0, 3, 6),
    Vector(1, 4, 7),
    Vector(2, 5, 8)
  )

  private val diags = Vector(
    Vector(0, 4, 8),
    Vector(2, 4, 6)
  )

  private val allCombinations = rows ++ cols ++ diags

  private val winningScore = 10.0
  private val losingScore = -10.0

  override def edgesOf(node: TicTacToeState): Iterable[TicTacToeAction] = {
    val score = assess(node)
    if (score == winningScore || score == losingScore) Vector.empty else {
      val piece = node.nextPlayer match {
        case XPlayer => X
        case OPlayer => O
      }
      node.pieces.indices.filter(i => node.pieces(i) == -
      ).map(i => TicTacToeAction(piece, i))
    }
  }

  override def assess(node: TicTacToeState): Double = {
    val winWithX = allCombinations.exists(one => one.forall(node.pieces(_) == X))
    if (winWithX) {
      if (root.nextPlayer == XPlayer) winningScore else losingScore
    } else {
      val winWithO = allCombinations.exists(one => one.forall(node.pieces(_) == O))
      if (winWithO) {
        if (root.nextPlayer == XPlayer) losingScore else winningScore
      } else {
        0
      }
    }
  }

  override def childOf(node: TicTacToeState, edge: TicTacToeAction): TicTacToeState = {
    node.copy(pieces = node.pieces.updated(edge.position, edge.piece), nextPlayer = node.nextPlayer.oppo)
  }
}
