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
  private val indices = (1 to 200).map(_ => random.nextLong().abs % 7)

  override def reactTo(state: CaribbeanState): Vector[CaribbeanAction] = {
    state.shipsOf(playerId).map(ship => {
      indices(state.turn - 1) match {
        case _ => Wait(ship.id)
      }
    })
  }
}
