package com.truelaurel.codingame.caribbean.offline

import com.truelaurel.codingame.caribbean.common.{FixedCabribbeanPlayer, _}
import com.truelaurel.codingame.caribbean.head.CaribbeanPlayer
import com.truelaurel.codingame.engine.GameSimulator
import com.truelaurel.codingame.hexagons.Offset

/**
  * Created by hwang on 15/04/2017.
  */
object CaribbeanArenaDebug {

  def main(args: Array[String]): Unit = {
    val state = CaribbeanState(CaribbeanContext(Map(), Map()),
      Vector(
        Ship(0, Offset(4, 1), 4, 0, 100, 1),
        Ship(2, Offset(17, 6), 1, 0, 100, 1),
        Ship(1, Offset(4, 19), 2, 0, 100, 0),
        Ship(3, Offset(17, 14), 5, 0, 100, 0)
      ).map(e => e.id -> e).toMap,
      Vector(
        Barrel(12, Offset(2, 12), 18),
        Barrel(11, Offset(2, 8), 18),
        Barrel(14, Offset(6, 14), 19),
        Barrel(13, Offset(6, 6), 19),
        Barrel(16, Offset(6, 16), 11),
        Barrel(15, Offset(6, 4), 11),
        Barrel(18, Offset(15, 17), 17),
        Barrel(17, Offset(15, 3), 17),
        Barrel(21, Offset(7, 13), 15),
        Barrel(20, Offset(7, 7), 15),
        Barrel(23, Offset(2, 18), 15),
        Barrel(22, Offset(2, 2), 15),
        Barrel(25, Offset(4, 17), 10),
        Barrel(24, Offset(4, 3), 10),
        Barrel(27, Offset(6, 13), 10),
        Barrel(26, Offset(6, 7), 10),
        Barrel(28, Offset(13, 10), 10),
        Barrel(31, Offset(12, 13), 14),
        Barrel(30, Offset(12, 7), 14),
        Barrel(34, Offset(1, 15), 16),
        Barrel(33, Offset(1, 5), 16)).map(e => e.id -> e).toMap,
      Map.empty,
      Map(7 -> Mine(7, Offset(4, 5))), 1)

    val myPlayer = CaribbeanPlayer(1, 0)
    val otherPlayer = FixedCabribbeanPlayer(0, 1)
    (0 until 200).foreach(_ => {
      val after = GameSimulator.simulate(200, state, CaribbeanArena, Vector(myPlayer, otherPlayer))
      println(after)
    })
  }
}
