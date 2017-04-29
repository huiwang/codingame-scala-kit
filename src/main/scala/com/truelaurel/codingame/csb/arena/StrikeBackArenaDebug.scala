package com.truelaurel.codingame.csb.arena

import com.truelaurel.codingame.csb.best.BestStrikeBackPlayer
import com.truelaurel.codingame.csb.model.{CheckPoint, Pod, StrikeBackContext, StrikeBackState}
import com.truelaurel.codingame.game.GameSimulator
import com.truelaurel.codingame.vectorial.Vectorl

/**
  * Created by hwang on 29/04/2017.
  */
object StrikeBackArenaDebug {

  def main(args: Array[String]): Unit = {
    val state = ???
    val me = BestStrikeBackPlayer(StrikeBackContext.me)
    val other = BestStrikeBackPlayer(StrikeBackContext.other)
    val predicted = GameSimulator.singleTurn(state, StrikeBackArena, Vector(me, other))

    println(state)
    println(predicted)
  }
}
