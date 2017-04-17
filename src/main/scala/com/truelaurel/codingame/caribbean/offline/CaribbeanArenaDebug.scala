package com.truelaurel.codingame.caribbean.offline

import com.truelaurel.codingame.caribbean.best.BestCabribbeanPlayer
import com.truelaurel.codingame.caribbean.common._
import com.truelaurel.codingame.caribbean.head.CaribbeanPlayer
import com.truelaurel.codingame.engine.GameSimulator
import com.truelaurel.codingame.hexagons.Offset

/**
  * Created by hwang on 15/04/2017.
  */
object CaribbeanArenaDebug {

  def main(args: Array[String]): Unit = {
    val state = CaribbeanState(CaribbeanContext(Map(), Map()), Vector(Ship(0, Offset(1, 1), 4, 0, 100, 1), Ship(1, Offset(1, 19), 2, 0, 100, 0), Ship(2, Offset(11, 1), 4, 0, 100, 1), Ship(3, Offset(11, 19), 2, 0, 100, 0), Ship(4, Offset(16, 4), 2, 0, 100, 1), Ship(5, Offset(16, 16), 4, 0, 100, 0)), Vector(Barrel(14, Offset(10, 17), 19), Barrel(13, Offset(10, 3), 19), Barrel(16, Offset(8, 16), 11), Barrel(15, Offset(8, 4), 11), Barrel(18, Offset(2, 12), 16), Barrel(17, Offset(2, 8), 16), Barrel(21, Offset(19, 11), 14), Barrel(20, Offset(19, 9), 14), Barrel(23, Offset(11, 17), 18), Barrel(22, Offset(11, 3), 18), Barrel(25, Offset(17, 19), 16), Barrel(24, Offset(17, 1), 16), Barrel(27, Offset(15, 19), 19), Barrel(26, Offset(15, 1), 19)), Vector(), Vector(Mine(9, Offset(17, 5))), 1)
    val myPlayer = CaribbeanPlayer(1, 0)
    val otherPlayer = BestCabribbeanPlayer(0, 1)
    (0 until 100).foreach(_ => {
      val after = GameSimulator.simulate(200, state, CaribbeanArena, Vector(myPlayer, otherPlayer))
      println(after)
    })
  }
}
