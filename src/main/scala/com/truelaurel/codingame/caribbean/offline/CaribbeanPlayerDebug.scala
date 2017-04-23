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
    val state = CaribbeanState(CaribbeanContext(Map(),Map(2 -> 50, 0 -> 39)),Map(0 -> Ship(0,Offset(8,12),1,2,64,1), 2 -> Ship(2,Offset(10,11),2,1,50,1), 1 -> Ship(1,Offset(20,10),4,2,67,0), 3 -> Ship(3,Offset(17,16),5,2,59,0)),Map(),Map(59 -> Ball(59,Offset(12,14),3,1), 60 -> Ball(60,Offset(14,17),2,2)),Map(5 -> Mine(5,Offset(14,12)), 29 -> Mine(29,Offset(7,8)), 61 -> Mine(61,Offset(15,12)), 38 -> Mine(38,Offset(4,10)), 7 -> Mine(7,Offset(12,16)), 51 -> Mine(51,Offset(14,10)), 4 -> Mine(4,Offset(14,8))),51)

    val player = CaribbeanPlayer(1, 0, new CountStopper(100))
    println(player.reactTo(state))
    println(player.reactTo(state))
  }
}
