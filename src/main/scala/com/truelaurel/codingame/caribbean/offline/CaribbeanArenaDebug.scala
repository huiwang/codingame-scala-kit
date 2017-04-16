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
    val state = CaribbeanState(CaribbeanContext(Map(),Map()),Vector(Ship(0,Offset(7,8),0,0,100,1), Ship(1,Offset(7,12),0,0,100,0), Ship(2,Offset(12,6),2,0,100,1), Ship(3,Offset(12,14),4,0,100,0)),Vector(Barrel(14,Offset(10,14),11), Barrel(13,Offset(10,6),11), Barrel(16,Offset(8,14),16), Barrel(15,Offset(8,6),16), Barrel(19,Offset(5,17),20), Barrel(18,Offset(5,3),20), Barrel(21,Offset(9,15),13), Barrel(20,Offset(9,5),13), Barrel(23,Offset(1,14),16), Barrel(22,Offset(1,6),16), Barrel(24,Offset(7,10),18), Barrel(26,Offset(21,11),12), Barrel(25,Offset(21,9),12)),Vector(),Vector(),1)

    val myPlayer = CaribbeanPlayer(1, 0)
    val otherPlayer = BestCabribbeanPlayer(0, 1)
    val after = GameSimulator.simulate(200, state, CaribbeanArena, Vector(myPlayer, otherPlayer))
    println(after)
  }
}
