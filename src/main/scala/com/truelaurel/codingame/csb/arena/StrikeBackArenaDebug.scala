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
    val state = StrikeBackState(Vector(CheckPoint(0,Vectorl(4069.0,4654.0)), CheckPoint(1,Vectorl(13046.0,1911.0)), CheckPoint(2,Vectorl(6539.0,7863.0))),Vector(Pod(0,Vectorl(5515.0,3787.0),Vectorl(340.0,-83.0),Vectorl(0.9702957262759965,-0.24192189559966787),1), Pod(1,Vectorl(5756.0,4573.0),Vectorl(329.0,-120.0),Vectorl(0.9396926207859081,-0.34202014332566943),1), Pod(2,Vectorl(5255.0,2996.0),Vectorl(347.0,-47.0),Vectorl(0.9902680687415703,-0.13917310096006588),1), Pod(3,Vectorl(5982.0,5370.0),Vectorl(316.0,-153.0),Vectorl(0.8987940462991668,-0.4383711467890778),1)))
    val me = BestStrikeBackPlayer(StrikeBackContext.me)
    val other = BestStrikeBackPlayer(StrikeBackContext.other)
    val predicted = GameSimulator.singleTurn(state, StrikeBackArena, Vector(me, other))

    println(state)
    println(predicted)
  }
}
