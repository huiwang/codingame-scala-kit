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
    val state = CaribbeanState(CaribbeanContext(Map(),Map()),Vector(Ship(0,Offset(9,3),5,1,97,1), Ship(1,Offset(10,18),0,1,100,0)),Vector(Barrel(9,Offset(9,15),18), Barrel(8,Offset(9,5),18), Barrel(11,Offset(7,12),12), Barrel(10,Offset(7,8),12), Barrel(13,Offset(6,16),14), Barrel(12,Offset(6,4),14), Barrel(15,Offset(10,12),18), Barrel(14,Offset(10,8),18), Barrel(17,Offset(17,13),17), Barrel(16,Offset(17,7),17), Barrel(18,Offset(11,2),17)),Vector(),Vector(Mine(4,Offset(11,3))),4)

    val player = CaribbeanPlayer(1, 0, new CountStopper(100))
    println(player.reactTo(state))
    println(player.reactTo(state))
  }
}
