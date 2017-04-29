package com.truelaurel.codingame.csb.head

import com.truelaurel.codingame.csb.model.{CheckPoint, Pod, StrikeBackContext, StrikeBackState}
import com.truelaurel.codingame.vectorial.Vectorl

/**
  * Created by hwang on 29/04/2017.
  */
object StrikeBackPlayerDebug {

  def main(args: Array[String]): Unit = {
    val state = ???

    val player = StrikeBackPlayer(StrikeBackContext.me, StrikeBackContext.other)

    val actions = player.reactTo(state)
    println(actions)
  }
}
