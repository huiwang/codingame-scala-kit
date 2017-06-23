package com.truelaurel.samplegames.wondev

import com.truelaurel.codingame.challenge.GameController

import scala.io.StdIn._

object WondevController extends GameController[WondevContext, WondevState, Action] {
  def readContext: WondevContext = {
    val size = readInt
    val unitsperplayer = readInt
    WondevContext(size, unitsperplayer)
  }

  def readState(turn: Int, context: WondevContext): WondevState =
    WondevState.read(context).copy(turn = turn)

  def nextContext(context: WondevContext, state: WondevState, actions: Vector[Action]): WondevContext = {
    context
  }
}

case class WondevContext(size: Int, unitsperplayer: Int)





