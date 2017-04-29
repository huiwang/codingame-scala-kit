package com.truelaurel.codingame.caribbean.common

import com.truelaurel.codingame.game.GamePlayer

/**
  * Created by hwang on 14/04/2017.
  */
case class FixedCabribbeanPlayer(playerId: Int, otherPlayer: Int) extends GamePlayer[CaribbeanState, CaribbeanAction] {

  override def reactTo(state: CaribbeanState): Vector[CaribbeanAction] = {
    val otherShips = state.shipsOf(otherPlayer)
    state.shipsOf(playerId).map(ship => {
      if (otherShips.isEmpty) Wait(ship.id) else {
        val target = otherShips.minBy(other => other.center.distanceTo(ship.center)).center.toOffset
        Fire(ship.id, target)
      }
    })
  }
}
