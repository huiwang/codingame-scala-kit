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
    val state = CaribbeanState(CaribbeanContext(Map(),Map()),Vector(Ship(0,Offset(2,6),1,0,100,0), Ship(1,Offset(2,14),5,0,100,1), Ship(2,Offset(8,3),4,0,100,0), Ship(3,Offset(8,17),2,0,100,1), Ship(4,Offset(16,2),5,0,100,0), Ship(5,Offset(16,18),1,0,100,1)),Vector(Barrel(12,Offset(18,10),10), Barrel(14,Offset(12,12),15), Barrel(13,Offset(12,8),15), Barrel(16,Offset(11,14),17), Barrel(15,Offset(11,6),17), Barrel(18,Offset(3,17),11), Barrel(17,Offset(3,3),11), Barrel(20,Offset(1,14),11), Barrel(19,Offset(1,6),11), Barrel(22,Offset(13,18),15), Barrel(21,Offset(13,2),15), Barrel(24,Offset(9,12),15), Barrel(23,Offset(9,8),15)),Vector(),Vector(Mine(7,Offset(2,16)), Mine(9,Offset(20,16))),1)
    val myPlayer = CaribbeanPlayer(1, 0)
    val otherPlayer = FixedCabribbeanPlayer(0, 1)
    (0 until 1).foreach(_ => {
      val after = GameSimulator.simulate(200, state, CaribbeanArena, Vector(myPlayer, otherPlayer))
      println(after)
    })
  }
}
