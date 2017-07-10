package com.truelaurel.samplegames.wondev.domain

import com.truelaurel.codingame.challenge.GameAccumulator

/**
  * Created by hwang on 09/07/2017.
  */
object WondevAccumulator extends GameAccumulator[WondevState, WondevAction] {
  override def accumulate(previous: WondevState, actions: Vector[WondevAction]): WondevState = previous
}
