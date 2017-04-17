package com.truelaurel.codingame.caribbean.best

import com.truelaurel.codingame.caribbean.common._
import com.truelaurel.codingame.engine.GamePlayer

import scala.util.Random

/**
  * Created by hwang on 14/04/2017.
  */
case class BestCabribbeanPlayer(playerId: Int, otherPlayer: Int) extends GamePlayer[CaribbeanState, CaribbeanAction] {
  val seed = 1
  val random = new Random(seed)
  val indices = (1 to 200).map(_ => random.nextLong().abs % 7)

  override def reactTo(state: CaribbeanState, timeElapsed: Long): Vector[CaribbeanAction] = {
    state.shipsOf(playerId).map(ship => {
      indices(state.turn - 1) match {
        case 0 => Faster(ship.id)
        case 1 => Slower(ship.id)
        case 2 => Port(ship.id)
        case 3 => Starboard(ship.id)
        case 4 => Wait(ship.id)
        case 5 => Faster(ship.id)
        case 6 => Faster(ship.id)
      }
    })
  }
}
