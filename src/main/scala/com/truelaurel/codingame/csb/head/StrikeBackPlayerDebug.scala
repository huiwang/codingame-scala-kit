package com.truelaurel.codingame.csb.head

import com.truelaurel.codingame.csb.model.{CheckPoint, Pod, StrikeBackContext, StrikeBackState}
import com.truelaurel.codingame.vectorial.Vectorl

/**
  * Created by hwang on 29/04/2017.
  */
object StrikeBackPlayerDebug {

  def main(args: Array[String]): Unit = {
    val state = StrikeBackState(Vector(CheckPoint(0,Vectorl(4069.0,4654.0)), CheckPoint(1,Vectorl(13046.0,1911.0)), CheckPoint(2,Vectorl(6539.0,7863.0))),Vector(Pod(0,Vectorl(3923.0,4176.0),Vectorl(0.0,0.0),Vectorl(0.970535462692705,-0.24095832763334177),1,1.0), Pod(1,Vectorl(4215.0,5132.0),Vectorl(0.0,0.0),Vectorl(0.9394606319681904,-0.34265685602644563),1,1.0), Pod(2,Vectorl(3631.0,3219.0),Vectorl(0.0,0.0),Vectorl(0.9904870959429274,-0.1376056422191555),1,1.0), Pod(3,Vectorl(4507.0,6089.0),Vectorl(0.0,0.0),Vectorl(0.8982441135466412,-0.43949688563038614),1,1.0)))

    val player = StrikeBackPlayer(StrikeBackContext.me, StrikeBackContext.other)

    val actions = player.reactTo(state)
    println(actions)
  }
}
