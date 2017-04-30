package com.truelaurel.codingame.csb.head

import com.truelaurel.codingame.csb.model.{CheckPoint, Pod, StrikeBackContext, StrikeBackState}
import com.truelaurel.codingame.time.CountStopper
import com.truelaurel.codingame.vectorial.Vectorl

/**
  * Created by hwang on 29/04/2017.
  */
object StrikeBackPlayerDebug {

  def main(args: Array[String]): Unit = {
    val state = StrikeBackState(StrikeBackContext(Vector(CheckPoint(0,Vectorl(12923.0,7212.0)), CheckPoint(1,Vectorl(5613.0,2603.0)), CheckPoint(2,Vectorl(4075.0,7438.0)), CheckPoint(3,Vectorl(13504.0,2317.0))),Vector(Pod(0,Vectorl(7310.0,6948.0),Vectorl(-148.0,605.0),Vectorl(-0.8910065241883678,0.45399049973954686),6,1.0), Pod(1,Vectorl(13465.0,7713.0),Vectorl(-331.0,150.0),Vectorl(-0.981627183447664,-0.19080899537654472),3,1.0), Pod(2,Vectorl(4409.0,8029.0),Vectorl(213.0,529.0),Vectorl(0.8910065241883679,0.45399049973954675),7,1.0), Pod(3,Vectorl(10881.0,4813.0),Vectorl(686.0,-544.0),Vectorl(0.656059028990507,-0.7547095802227721),7,1.0)),Vector(0, 0, 0, 0)),Vector(Pod(0,Vectorl(6972.0,7615.0),Vectorl(-287.0,566.0),Vectorl(-0.9510565162951535,0.3090169943749475),6,1.0), Pod(1,Vectorl(12946.0,7795.0),Vectorl(-441.0,69.0),Vectorl(-0.9396926207859084,-0.34202014332566866),3,1.0), Pod(2,Vectorl(4786.0,8584.0),Vectorl(320.0,471.0),Vectorl(0.9876883405951378,0.15643446504023087),7,1.0), Pod(3,Vectorl(11567.0,4269.0),Vectorl(583.0,-462.0),Vectorl(0.8571673007021121,-0.5150380749100545),7,1.0)),87)

    val player = StrikeBackPlayer(StrikeBackContext.me, StrikeBackContext.other, new CountStopper(200))
    val actions = player.reactTo(state)
    println(actions)
  }
}
