package com.truelaurel.samplegames.wondev.strategy

import com.truelaurel.samplegames.wondev.domain.{WondevAction, WondevState}

case class WondevBot(side: Boolean) {
  def react(state: WondevState): Vector[WondevAction] = {
    ???
  }


}
