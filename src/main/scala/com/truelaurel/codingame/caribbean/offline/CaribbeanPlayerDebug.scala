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
    val state = ???

    val player = CaribbeanPlayer(1, 0, new CountStopper(100))
    println(player.reactTo(state))
    println(player.reactTo(state))
  }
}
