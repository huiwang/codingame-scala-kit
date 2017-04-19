package com.truelaurel.codingame.caribbean.best

import com.truelaurel.codingame.caribbean.common._
import com.truelaurel.codingame.engine.GamePlayer

import scala.util.Random

/**
  * Created by hwang on 14/04/2017.
  */
case class WaitingCabribbeanPlayer(playerId: Int, otherPlayer: Int) extends GamePlayer[CaribbeanState, CaribbeanAction] {
  private val seed = 1
  private val random = new Random(seed)

  override def reactTo(state: CaribbeanState): Vector[CaribbeanAction] = {
    state.shipsOf(playerId).map(ship => {
      random.nextInt(3) match {
        case 0 => Port(ship.id)
        case 1 => Starboard(ship.id)
        case 2 => Faster(ship.id)
      }
    })
  }
}
