package com.truelaurel.codingame.csb.head

import com.truelaurel.codingame.csb.model.{CheckPoint, Pod, StrikeBackContext, StrikeBackState}
import com.truelaurel.codingame.time.CountStopper
import com.truelaurel.codingame.vectorial.Vectorl

/**
  * Created by hwang on 29/04/2017.
  */
object StrikeBackPlayerDebug {

  def main(args: Array[String]): Unit = {
    val state = StrikeBackState(StrikeBackContext(Vector(CheckPoint(0,Vectorl(12923.0,7212.0)), CheckPoint(1,Vectorl(5613.0,2603.0)), CheckPoint(2,Vectorl(4075.0,7438.0)), CheckPoint(3,Vectorl(13504.0,2317.0))),Vector(Pod(0,Vectorl(12839.0,6049.0),Vectorl(352.0,-23.0),Vectorl(0.9510565162951535,0.3090169943749474),1,1.0), Pod(1,Vectorl(13220.0,2807.0),Vectorl(64.0,-143.0),Vectorl(0.9612616959383189,-0.27563735581699894),4,1.0), Pod(2,Vectorl(16208.0,4678.0),Vectorl(-248.0,447.0),Vectorl(-0.9063077870366499,0.4226182617406995),4,1.0), Pod(3,Vectorl(15375.0,3184.0),Vectorl(-32.0,239.0),Vectorl(-0.587785252292473,0.8090169943749475),4,1.0)),Vector(0, 0, 0, 0)),Vector(Pod(0,Vectorl(13236.0,6049.0),Vectorl(337.0,0.0),Vectorl(0.8910065241883679,0.45399049973954675),1,1.0), Pod(1,Vectorl(13484.0,2671.0),Vectorl(224.0,-115.0),Vectorl(0.9993908270190958,0.03489949670250097),4,1.0), Pod(2,Vectorl(15778.0,5203.0),Vectorl(-365.0,446.0),Vectorl(-0.9205048534524404,0.39073112848927377),4,1.0), Pod(3,Vectorl(15225.0,3585.0),Vectorl(-127.0,340.0),Vectorl(-0.587785252292473,0.8090169943749475),4,1.0)),47)
    val player = StrikeBackPlayer(StrikeBackContext.me, StrikeBackContext.other, new CountStopper(200))
    val actions = player.reactTo(state)
    println(actions)
  }
}
