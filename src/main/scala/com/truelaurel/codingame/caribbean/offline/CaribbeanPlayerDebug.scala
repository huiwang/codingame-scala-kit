package com.truelaurel.codingame.caribbean.offline

import com.truelaurel.codingame.caribbean.common._
import com.truelaurel.codingame.caribbean.head.CaribbeanPlayer
import com.truelaurel.codingame.engine.GameSimulator
import com.truelaurel.codingame.hexagons.Offset
import com.truelaurel.codingame.time.CountStopper

/**
  * Created by hwang on 15/04/2017.
  */
object CaribbeanPlayerDebug {

  def main(args: Array[String]): Unit = {
    val state = CaribbeanState(CaribbeanContext(Map(),Map()),Vector(Ship(0,Offset(7,8),0,2,99,1), Ship(1,Offset(3,14),1,0,94,0)),Vector(Barrel(11,Offset(20,17),18), Barrel(10,Offset(20,3),18), Barrel(13,Offset(13,15),13), Barrel(12,Offset(13,5),13), Barrel(15,Offset(10,18),15), Barrel(14,Offset(10,2),15), Barrel(17,Offset(10,11),18), Barrel(16,Offset(10,9),18), Barrel(19,Offset(19,14),14), Barrel(18,Offset(19,6),14), Barrel(21,Offset(5,13),19), Barrel(23,Offset(4,16),14), Barrel(22,Offset(4,4),14), Barrel(25,Offset(15,18),11), Barrel(24,Offset(15,2),11)),Vector(),Vector(Mine(7,Offset(4,13)), Mine(6,Offset(4,7)), Mine(8,Offset(9,5))),7)
    val player = CaribbeanPlayer(1, 0)
    player.reactTo(state)
    println(player.reactTo(state))
  }
}
