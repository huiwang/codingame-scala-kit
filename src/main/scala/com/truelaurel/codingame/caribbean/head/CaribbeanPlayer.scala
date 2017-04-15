package com.truelaurel.codingame.caribbean.head

import com.truelaurel.codingame.caribbean.common._
import com.truelaurel.codingame.engine.GamePlayer

/**
  * Created by hwang on 14/04/2017.
  */
case class CaribbeanPlayer(playerId: Int, otherPlayer: Int) extends GamePlayer[CaribbeanState, CaribbeanAction] {
  override def reactTo(state: CaribbeanState): Vector[CaribbeanAction] = {
    val myShips = state.shipsOf(playerId)
    val otherShips = state.shipsOf(otherPlayer)
    myShips.map(myShip => {
      val fire = otherShips.map(os => (myShip, os)).find { case (my, other) =>
        (my.rums <= other.rums || state.barrels.isEmpty) &&
          (my.speed != 0 || (other.speed == 0 || state.barrels.isEmpty)) &&
          my.center.distanceTo(other.center) <= 10
      }.map { case (my, other) => Fire(my.id, other.position) }

      if (fire.isDefined) {
        fire.get
      } else if (state.barrels.nonEmpty) {
        val barrel = state.barrels.minBy(b => (b.cube.distanceTo(myShip.center), b.position.x))
        Move(myShip.id, barrel.position)
      } else {
        Wait(myShip.id)
      }
    })
  }
}
