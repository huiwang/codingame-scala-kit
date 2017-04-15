package com.truelaurel.codingame.caribbean.head

import com.truelaurel.codingame.caribbean.common._
import com.truelaurel.codingame.engine.GamePlayer
import com.truelaurel.codingame.hexagons.Cube

/**
  * Created by hwang on 14/04/2017.
  */
case class CabribbeanPlayer(playerId: Int, otherPlayer: Int) extends GamePlayer[CaribbeanState, CaribbeanAction] {
  override def reactTo(state: CaribbeanState): Vector[CaribbeanAction] = {
    val myShips = state.shipsOf(playerId)
    val otherShips = state.shipsOf(otherPlayer)
    myShips.map(myShip => {
      val fire = otherShips.map(os => (myShip, os)).find { case (my, other) =>
        (my.rums <= other.rums || state.barrels.isEmpty) &&
          (my.speed != 0 || (other.speed == 0 || state.barrels.isEmpty)) &&
          my.center.distanceTo(other.center) <= 10
      }.map { case (my, other) => Fire(other.x, other.y) }

      if (fire.isDefined) {
        fire.get
      } else if (state.barrels.nonEmpty) {
        val barrel = state.barrels.minBy(b => (Cube(b.x, b.y).distanceTo(Cube(myShip.x, myShip.y)), b.x))
        Move(barrel.x, barrel.y)
      } else {
        Wait
      }
    })
  }
}
