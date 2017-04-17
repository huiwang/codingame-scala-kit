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

  it should "predict rotation" in {
    val state = CaribbeanState(
      context = CaribbeanContext(),
      ships = Vector(
        Ship(0, Offset(7, 7), orientation = 0, speed = 1, rums = 10, owner = 1),
        Ship(1, Offset(18, 7), orientation = 3, speed = 1, rums = 10, owner = 0)
      ),
      barrels = Vector(
        Barrel(2, Offset(10, 7), 20),
        Barrel(3, Offset(17, 8), 20)
      ),
      balls = Vector.empty,
      mines = Vector.empty,
      turn = 1
    )

    CaribbeanArena.next(state, Vector(Faster(0), Port(1))) should be(
      CaribbeanState(
        context = CaribbeanContext(),
        ships = Vector(
          Ship(0, Offset(9, 7), orientation = 0, speed = 2, rums = 29, owner = 1),
          Ship(1, Offset(17, 7), orientation = 4, speed = 1, rums = 29, owner = 0)
        ),
        barrels = Vector.empty,
        balls = Vector.empty,
        mines = Vector.empty,
        turn = 2
      )
    )
  }

  it should "predict mine damage" in {
    val state = CaribbeanState(
      context = CaribbeanContext(),
      ships = Vector(
        Ship(0, Offset(7, 7), orientation = 0, speed = 1, rums = 30, owner = 1),
        Ship(1, Offset(13, 8), orientation = 3, speed = 1, rums = 30, owner = 0)
      ),
      barrels = Vector(
        Barrel(2, Offset(16, 8), 20),
        Barrel(3, Offset(17, 8), 20)
      ),
      balls = Vector.empty,
      mines = Vector(
        Mine(4, Offset(11, 7))
      ),
      turn = 1
    )

    CaribbeanArena.next(state, Vector(Faster(0), Starboard(1))) should be(
      CaribbeanState(
        context = CaribbeanContext(),
        ships = Vector(
          Ship(0, Offset(9, 7), orientation = 0, speed = 2, rums = 19, owner = 1),
          Ship(1, Offset(12, 8), orientation = 2, speed = 1, rums = 4, owner = 0)
        ),
        barrels = Vector(
          Barrel(2, Offset(16, 8), 20),
          Barrel(3, Offset(17, 8), 20)
        ),
        balls = Vector.empty,
        mines = Vector.empty,
        turn = 2
      )
    )
  }

  it should "predict ship collision" in {
    val state = CaribbeanState(
      context = CaribbeanContext(),
      ships = Vector(
        Ship(0, Offset(7, 7), orientation = 0, speed = 1, rums = 30, owner = 1),
        Ship(1, Offset(13, 7), orientation = 3, speed = 1, rums = 30, owner = 0)
      ),
      barrels = Vector(
        Barrel(3, Offset(10, 7), 20)
      ),
      balls = Vector.empty,
      mines = Vector(),
      turn = 1
    )

    CaribbeanArena.next(state, Vector(Faster(0), Faster(1))) should be(
      CaribbeanState(
        context = CaribbeanContext(),
        ships = Vector(
          Ship(0, Offset(8, 7), orientation = 0, speed = 0, rums = 29, owner = 1),
          Ship(1, Offset(12, 7), orientation = 3, speed = 0, rums = 29, owner = 0)
        ),
        barrels = Vector(
          Barrel(3, Offset(10, 7), 20)
        ),
        balls = Vector.empty,
        mines = Vector.empty,
        turn = 2
      )
    )
  }


}
