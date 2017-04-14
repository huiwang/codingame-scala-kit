package com.truelaurel.codingame.caribbean.head

import com.truelaurel.codingame.caribbean.common.{CaribbeanAction, CaribbeanState, Move}
import com.truelaurel.codingame.engine.GamePlayer
import com.truelaurel.codingame.vectorial.Vectorl

/**
  * Created by hwang on 14/04/2017.
  */
case class CabribbeanPlayer(playerId: Int) extends GamePlayer[CaribbeanState, CaribbeanAction] {
  override def reactTo(state: CaribbeanState): Vector[CaribbeanAction] = {
    state.ships.map(s => {
      val barrel = state.barrels.minBy(b => (Vectorl(b.x, b.y) - Vectorl(s.x, s.y)).mag2)
      Move(barrel.x, barrel.y)
    })
  }
}
