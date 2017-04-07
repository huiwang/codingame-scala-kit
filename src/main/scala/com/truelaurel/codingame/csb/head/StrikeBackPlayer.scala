package com.truelaurel.codingame.csb.head

import com.truelaurel.codingame.csb.common.{PodAction, StrikeBackGameState, Thrust}
import com.truelaurel.codingame.engine.GamePlayer

/**
  * Created by hwang on 02/04/2017.
  */
case class StrikeBackPlayer(range: Vector[Int]) extends GamePlayer[StrikeBackGameState, PodAction] {

  override def reactTo(state: StrikeBackGameState): Vector[PodAction] = {
    range.map(i => {
      Thrust(state.checkPoints(state.nextCPs(i)).p, 100)
    })
  }
}
