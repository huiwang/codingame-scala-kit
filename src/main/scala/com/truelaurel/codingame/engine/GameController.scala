package com.truelaurel.codingame.engine

/**
  * The Game Controller reads init context and game turn.
  * In the context, we can store information not included in the input, for example cool down on actions.
  * It also provides a warmup method which can be implemented for perf-critical games.
  */
trait GameController[C, S, A] {
  def readContext: C

  def readState(turn: Int, context: C): S

  def nextContext(context: C, state: S, actions: Vector[A]): C

  def warmup(player: GamePlayer[S, A]): Unit = {}
}
