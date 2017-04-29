package com.truelaurel.codingame.csb.best

import com.truelaurel.codingame.csb.model.{StrikeBackAction, StrikeBackState, Thrust}
import com.truelaurel.codingame.engine.GamePlayer

/**
  * Created by hwang on 02/04/2017.
  */
case class BestStrikeBackPlayer(me: Int) extends GamePlayer[StrikeBackState, StrikeBackAction] {

  override def reactTo(state: StrikeBackState): Vector[StrikeBackAction] = {
    state.podsOf(me).map(pod => Thrust(pod.id, state.checkPoints(pod.goal).position, 100))
  }
}
