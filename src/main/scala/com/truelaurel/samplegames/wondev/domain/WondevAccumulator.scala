package com.truelaurel.samplegames.wondev.domain

import com.truelaurel.codingame.challenge.GameAccumulator

/**
  * Created by hwang on 09/07/2017.
  */
object WondevAccumulator extends GameAccumulator[WondevContext, WondevState, WondevAction]{
  override def accumulate(context: WondevContext, state: WondevState, action: WondevAction): WondevContext = context
}
