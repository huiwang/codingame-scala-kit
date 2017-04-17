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
    val state = CaribbeanState(CaribbeanContext(Map(),Map()),Vector(Ship(0,Offset(8,5),4,0,100,0), Ship(1,Offset(8,15),2,0,100,1), Ship(2,Offset(13,1),4,0,100,0), Ship(3,Offset(13,19),2,0,100,1)),Vector(Barrel(16,Offset(10,19),19), Barrel(15,Offset(10,1),19), Barrel(18,Offset(19,19),11), Barrel(17,Offset(19,1),11), Barrel(20,Offset(7,17),19), Barrel(19,Offset(7,3),19), Barrel(22,Offset(2,17),19), Barrel(21,Offset(2,3),19), Barrel(24,Offset(9,13),18), Barrel(23,Offset(9,7),18), Barrel(26,Offset(9,11),14), Barrel(25,Offset(9,9),14)),Vector(),Vector(Mine(5,Offset(8,18))),1)
    val myPlayer = CaribbeanPlayer(1, 0)
    val otherPlayer = BestCabribbeanPlayer(0, 1)
    (0 until 100).foreach(_ => {
      val after = GameSimulator.simulate(200, state, CaribbeanArena, Vector(myPlayer, otherPlayer))
      println(after)
    })
  }
}
