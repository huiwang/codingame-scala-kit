package com.truelaurel.problem.tictactoe.minimax

/**
  * Created by hwang on 18/06/2017.
  */
sealed trait TicTacToePiece

case object X extends TicTacToePiece

case object O extends TicTacToePiece

case object N extends TicTacToePiece

sealed trait TicTacToePlayer {
  def oppo: TicTacToePlayer
}

case object XPlayer extends TicTacToePlayer {
  override def oppo: TicTacToePlayer = OPlayer
}

case object OPlayer extends TicTacToePlayer {
  override def oppo: TicTacToePlayer = XPlayer
}

case class TicTacToeState(pieces: Vector[TicTacToePiece], nextPlayer: TicTacToePlayer) {

  def prettyPrint: String = pieces.sliding(3, 3).map(row => row.mkString(" ")).mkString("\n")
}

case class TicTacToeAction(piece: TicTacToePiece, position: Int)