package com.truelaurel.codingame.caribbean.offline

import com.truelaurel.codingame.caribbean.common._
import com.truelaurel.codingame.caribbean.head.CaribbeanPlayer
import com.truelaurel.codingame.engine.GameSimulator

/**
  * Created by hwang on 15/04/2017.
  */
object CaribbeanOneSingleTurn {

  def main(args: Array[String]): Unit = {
    val state = ???
    val player = CaribbeanPlayer(1, 0)
    println(player.reactTo(state))
  }
}
