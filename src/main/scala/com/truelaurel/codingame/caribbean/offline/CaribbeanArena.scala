package com.truelaurel.codingame.caribbean.offline

import com.truelaurel.codingame.caribbean.common._
import com.truelaurel.codingame.engine.{GameArena, GameResult}

/**
  * Created by hwang on 15/04/2017.
  */
class CaribbeanArena extends GameArena[CaribbeanState, CaribbeanAction] {
  override def next(state: CaribbeanState, actions: Vector[CaribbeanAction]): CaribbeanState = {
    ???
  }

  override def judge(state: CaribbeanState): GameResult = ???
}
