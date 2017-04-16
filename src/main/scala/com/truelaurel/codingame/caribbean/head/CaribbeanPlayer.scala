package com.truelaurel.codingame.caribbean.head

import com.truelaurel.codingame.caribbean.common._
import com.truelaurel.codingame.engine.GamePlayer

import scala.util.Random

/**
  * Created by hwang on 14/04/2017.
  */
case class CaribbeanPlayer(playerId: Int, otherPlayer: Int) extends GamePlayer[CaribbeanState, CaribbeanAction] {
  val seed = 123849127234712L
  val random = new Random(seed)

  override def reactTo(state: CaribbeanState): Vector[CaribbeanAction] = {
    state.shipsOf(playerId).map(ship => {
      random.nextLong().abs % 5 match {
        case 0 => Faster(ship.id)
        case 1 => Slower(ship.id)
        case 2 => Port(ship.id)
        case 3 => Starboard(ship.id)
        case 4 => Wait(ship.id)
      }
    })
  }
}
