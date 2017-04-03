package com.truelaurel.codingame.csb.offline

import com.truelaurel.codingame.csb.common.{PodAction, StrikeBackGameState}
import com.truelaurel.codingame.engine.{GameArena, GameResult}

/**
  * Created by hwang on 02/04/2017.
  */
object StrikeBackArena extends GameArena[StrikeBackGameState, PodAction] {
  override def next(fromState: StrikeBackGameState, actions: Vector[PodAction]): StrikeBackGameState = {
    ???
  }

  override def judge(state: StrikeBackGameState): GameResult = {
    ???
  }
}
