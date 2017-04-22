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
    val state = CaribbeanState(CaribbeanContext(Map(),Map(0 -> 12, 4 -> 13, 2 -> 21)),Map(0 -> Ship(0,Offset(3,11),4,1,60,1), 5 -> Ship(5,Offset(9,2),4,2,93,0), 1 -> Ship(1,Offset(8,11),4,2,88,0), 2 -> Ship(2,Offset(0,14),3,2,92,1), 3 -> Ship(3,Offset(9,7),5,2,98,0), 4 -> Ship(4,Offset(5,11),4,1,89,1)),Map(),Map(57 -> Ball(57,Offset(4,10),3,0), 59 -> Ball(59,Offset(4,10),5,1), 63 -> Ball(63,Offset(6,10),1,2), 58 -> Ball(58,Offset(6,10),1,1), 62 -> Ball(62,Offset(0,10),3,3)),Map(48 -> Mine(48,Offset(5,15))),27)

    val player = CaribbeanPlayer(1, 0, new CountStopper(100))
    println(player.reactTo(state))
    println(player.reactTo(state))
  }
}
