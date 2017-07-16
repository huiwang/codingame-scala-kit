package com.truelaurel.samplegames.wondev.analysis

import com.truelaurel.math.geometry.Pos
import com.truelaurel.samplegames.wondev.domain.{MoveBuild, WondevState}
import org.scalatest.FlatSpec

/**
  * Created by hwang on 16/07/2017.
  */
class WarFogAnalysisTest extends FlatSpec {

  behavior of "WarFogAnalysisTest"

  val meFirstState = WondevState(5,
    Map(
      Pos(0, 2) -> 48, Pos(0, 0) -> 48, Pos(4, 0) -> 48, Pos(3, 4) -> 48, Pos(3, 1) -> 48,
      Pos(4, 1) -> 48, Pos(2, 0) -> 48, Pos(0, 3) -> 48, Pos(4, 4) -> 48, Pos(3, 0) -> 48,
      Pos(1, 1) -> 48, Pos(1, 4) -> 48, Pos(0, 4) -> 48, Pos(3, 2) -> 48, Pos(1, 3) -> 48,
      Pos(2, 2) -> 48, Pos(4, 2) -> 48, Pos(2, 4) -> 48, Pos(0, 1) -> 48, Pos(3, 3) -> 48,
      Pos(2, 3) -> 48, Pos(1, 2) -> 48, Pos(2, 1) -> 48, Pos(4, 3) -> 48, Pos(1, 0) -> 48),
    List(Pos(0, 4), Pos(1, 0), Pos(0, 0), Pos(-1, -1)),
    List(
      MoveBuild(0, Pos(1, 4), Pos(2, 4)), MoveBuild(0, Pos(1, 4), Pos(1, 3)), MoveBuild(0, Pos(1, 4), Pos(2, 3)),
      MoveBuild(0, Pos(1, 4), Pos(0, 3)), MoveBuild(0, Pos(1, 4), Pos(0, 4)), MoveBuild(0, Pos(0, 3), Pos(1, 3)),
      MoveBuild(0, Pos(0, 3), Pos(0, 2)), MoveBuild(0, Pos(0, 3), Pos(1, 2)), MoveBuild(0, Pos(0, 3), Pos(0, 4)), MoveBuild(0, Pos(0, 3), Pos(1, 4)), MoveBuild(0, Pos(1, 3), Pos(2, 3)), MoveBuild(0, Pos(1, 3), Pos(1, 2)), MoveBuild(0, Pos(1, 3), Pos(2, 2)), MoveBuild(0, Pos(1, 3), Pos(0, 2)), MoveBuild(0, Pos(1, 3), Pos(1, 4)), MoveBuild(0, Pos(1, 3), Pos(2, 4)), MoveBuild(0, Pos(1, 3), Pos(0, 4)), MoveBuild(0, Pos(1, 3), Pos(0, 3)), MoveBuild(1, Pos(2, 0), Pos(3, 0)), MoveBuild(1, Pos(2, 0), Pos(2, 1)), MoveBuild(1, Pos(2, 0), Pos(3, 1)), MoveBuild(1, Pos(2, 0), Pos(1, 1)), MoveBuild(1, Pos(2, 0), Pos(1, 0)), MoveBuild(1, Pos(1, 1), Pos(2, 1)), MoveBuild(1, Pos(1, 1), Pos(1, 0)), MoveBuild(1, Pos(1, 1), Pos(2, 0)), MoveBuild(1, Pos(1, 1), Pos(1, 2)), MoveBuild(1, Pos(1, 1), Pos(2, 2)), MoveBuild(1, Pos(1, 1), Pos(0, 2)), MoveBuild(1, Pos(1, 1), Pos(0, 1)), MoveBuild(1, Pos(2, 1), Pos(3, 1)), MoveBuild(1, Pos(2, 1), Pos(2, 0)), MoveBuild(1, Pos(2, 1), Pos(3, 0)), MoveBuild(1, Pos(2, 1), Pos(1, 0)), MoveBuild(1, Pos(2, 1), Pos(2, 2)), MoveBuild(1, Pos(2, 1), Pos(3, 2)), MoveBuild(1, Pos(2, 1), Pos(1, 2)), MoveBuild(1, Pos(2, 1), Pos(1, 1)), MoveBuild(1, Pos(0, 1), Pos(1, 1)), MoveBuild(1, Pos(0, 1), Pos(1, 0)), MoveBuild(1, Pos(0, 1), Pos(0, 2)), MoveBuild(1, Pos(0, 1), Pos(1, 2))), true)


  it should "restrictOppoScope" in {


  }

}
