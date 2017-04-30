package com.truelaurel.codingame.csb.best

import com.truelaurel.codingame.csb.analysis.PodAnalysis
import com.truelaurel.codingame.csb.model.{StrikeBackAction, StrikeBackState, Thrust}
import com.truelaurel.codingame.game.GamePlayer

/**
  * Created by hwang on 02/04/2017.
  */
case class BestStrikeBackPlayer(player: Int) extends GamePlayer[StrikeBackState, StrikeBackAction] {

  override def reactTo(state: StrikeBackState): Vector[StrikeBackAction] = {
    state.podsOf(player).map(pod => Thrust(pod.id, PodAnalysis.goalOf(pod, state).position, 200))
  }
}
