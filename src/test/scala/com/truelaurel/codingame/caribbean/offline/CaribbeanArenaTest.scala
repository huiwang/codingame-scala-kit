package com.truelaurel.codingame.caribbean.offline

import com.truelaurel.codingame.caribbean.common._
import com.truelaurel.codingame.hexagons.Offset
import org.scalatest.{FlatSpec, Matchers}

/**
  * Created by hwang on 17/04/2017.
  */
class CaribbeanArenaTest extends FlatSpec with Matchers {

  behavior of "CaribbeanArenaTest"

  it should "predict acceleration" in {
    val state = CaribbeanState(
      context = CaribbeanContext(),
      ships = Vector(
        Ship(0, Offset(7, 7), orientation = 0, speed = 1, rums = 10, owner = 1),
        Ship(1, Offset(18, 7), orientation = 3, speed = 0, rums = 10, owner = 0)
      ),
      barrels = Vector(
        Barrel(2, Offset(10, 7), 20),
        Barrel(3, Offset(14, 7), 20)
      ),
      balls = Vector.empty,
      mines = Vector.empty,
      turn = 1
    )

    CaribbeanArena.next(state, Vector(Faster(0), Faster(1))) should be(
      CaribbeanState(
        context = CaribbeanContext(),
        ships = Vector(
          Ship(0, Offset(9, 7), orientation = 0, speed = 2, rums = 29, owner = 1),
          Ship(1, Offset(17, 7), orientation = 3, speed = 1, rums = 9, owner = 0)
        ),
        barrels = Vector(
          Barrel(3, Offset(14, 7), 20)
        ),
        balls = Vector.empty,
        mines = Vector.empty,
        turn = 2
      )
    )
  }

}
