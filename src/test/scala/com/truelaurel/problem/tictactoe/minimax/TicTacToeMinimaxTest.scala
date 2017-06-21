package com.truelaurel.problem.tictactoe.minimax

import com.truelaurel.codingame.minimax.MinimaxAlgorithm
import org.scalatest.{FlatSpec, Matchers}

/**
  * Created by hwang on 18/06/2017.
  */
class TicTacToeMinimaxTest extends FlatSpec with Matchers {

  behavior of "TicTacToeMinimaxTest"

  it should "it should find the move which leads to victory" in {

    val winningActions = Set(TicTacToeAction(O, 0), TicTacToeAction(O, 2))
    val root = TicTacToeState(
      Vector(
        N, O, N,
        X, O, X,
        N, X, N
      ), OPlayer
    )
    val repr = new TicTacToeMinimaxRepresentation(root)
    val minimax = new MinimaxAlgorithm[TicTacToeState, TicTacToeAction]()

    val suggestedAction = minimax.search(repr, 4)
    winningActions.contains(suggestedAction) shouldBe true
  }

}
