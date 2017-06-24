package com.truelaurel.samplegames.wondev.strategy

import com.truelaurel.codingame.challenge.GamePlayer
import com.truelaurel.samplegames.wondev.{Action, WondevState}

case class TyrchoPlayer(side: Boolean) extends GamePlayer[WondevState, Action] {
  override def reactTo(state: WondevState): Vector[Action] = {
    val actionToMaxHeight = state.legalActions.maxBy { a =>
      val applied = state.apply(a)
      val tgt = applied.myUnits.head + a.dir2
      (applied.height(applied.myUnits.head))//, applied.height(tgt))
    }
    Vector(actionToMaxHeight)
  }
}
