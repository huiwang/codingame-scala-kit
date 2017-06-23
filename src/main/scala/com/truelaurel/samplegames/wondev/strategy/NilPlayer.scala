package com.truelaurel.samplegames.wondev.strategy

import com.truelaurel.codingame.challenge.GamePlayer
import com.truelaurel.samplegames.wondev.{Action, WondevState}

case class NilPlayer(side: Boolean) extends GamePlayer[WondevState, Action] {
  override def reactTo(state: WondevState): Vector[Action] = Vector(state.legalActions.head)
}
