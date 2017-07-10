package com.truelaurel.samplegames.wondev.strategy

import com.truelaurel.codingame.challenge.GameBot
import com.truelaurel.samplegames.wondev.domain.{WondevAction, WondevState}

case class WondevBot(side: Boolean) extends GameBot[WondevState, WondevAction] {
  override def react(state: WondevState): WondevAction = {
    ???
  }


}
