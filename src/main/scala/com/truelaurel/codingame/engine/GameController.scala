package com.truelaurel.codingame.engine

/**
  * Created by hwang on 15/04/2017.
  */
trait GameController[C, S, A] {
  def readContext: C

  def readState(turn: Int, context: C): S

  def nextContext(context: C, state: S, actions: Vector[A]): C

  def warmup(player: GamePlayer[S, A]): Unit = {}
}
