package com.truelaurel.samplegames.wondev

import com.truelaurel.codingame.challenge.GameController


object WondevController extends GameController[WondevContext, WondevState, Action] {
  def readContext: WondevContext =
    WondevContext()

  def readState(turn: Int, context: WondevContext): WondevState = {
    WondevState.read().copy(
      turn = turn)
  }

  def nextContext(context: WondevContext, state: WondevState, actions: Vector[Action]): WondevContext = {
    context
  }
}

case class WondevContext()

case class WondevState(turn: Int)

object WondevState {
  def read() = WondevState(0)
}

trait Action