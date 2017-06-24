package com.truelaurel.samplegames.wondev.strategy

import com.truelaurel.codingame.challenge.GamePlayer
import com.truelaurel.samplegames.wondev.domain.{WondevAction, WondevState}

case class WondevPlayer(side: Boolean) extends GamePlayer[WondevState, WondevAction] {
  override def reactTo(state: WondevState): Vector[WondevAction] = {
    ???
  }


}
