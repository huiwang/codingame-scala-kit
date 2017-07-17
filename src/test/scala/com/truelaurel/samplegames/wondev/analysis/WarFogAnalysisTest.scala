package com.truelaurel.samplegames.wondev.analysis

import com.truelaurel.math.geometry.Pos
import com.truelaurel.samplegames.wondev.domain.{MoveBuild, WondevState}
import org.scalatest.{FlatSpec, Matchers}

/**
  * Created by hwang on 16/07/2017.
  */
class WarFogAnalysisTest extends FlatSpec with Matchers {

  behavior of "WarFogAnalysisTest"

  val meFirstState = WondevState(5,
    heightMap = Map(
      Pos(0, 2) -> 0, Pos(0, 0) -> 0, Pos(4, 0) -> 0, Pos(3, 4) -> 0, Pos(3, 1) -> 0,
      Pos(4, 1) -> 0, Pos(2, 0) -> 0, Pos(0, 3) -> 0, Pos(4, 4) -> 0, Pos(3, 0) -> 0,
      Pos(1, 1) -> 0, Pos(1, 4) -> 0, Pos(0, 4) -> 0, Pos(3, 2) -> 0, Pos(1, 3) -> 0,
      Pos(2, 2) -> 0, Pos(4, 2) -> 0, Pos(2, 4) -> 0, Pos(0, 1) -> 0, Pos(3, 3) -> 0,
      Pos(2, 3) -> 0, Pos(1, 2) -> 0, Pos(2, 1) -> 0, Pos(4, 3) -> 0, Pos(1, 0) -> 0),
    units = List(Pos(0, 4), Pos(1, 0), Pos(0, 0), Pos(-1, -1)),
    legalActions = List(
      MoveBuild(0, Pos(1, 4), Pos(2, 4)), MoveBuild(0, Pos(1, 4), Pos(1, 3)), MoveBuild(0, Pos(1, 4), Pos(2, 3)),
      MoveBuild(0, Pos(1, 4), Pos(0, 3)), MoveBuild(0, Pos(1, 4), Pos(0, 4)), MoveBuild(0, Pos(0, 3), Pos(1, 3)),
      MoveBuild(0, Pos(0, 3), Pos(0, 2)), MoveBuild(0, Pos(0, 3), Pos(1, 2)), MoveBuild(0, Pos(0, 3), Pos(0, 4)),
      MoveBuild(0, Pos(0, 3), Pos(1, 4)), MoveBuild(0, Pos(1, 3), Pos(2, 3)), MoveBuild(0, Pos(1, 3), Pos(1, 2)),
      MoveBuild(0, Pos(1, 3), Pos(2, 2)), MoveBuild(0, Pos(1, 3), Pos(0, 2)), MoveBuild(0, Pos(1, 3), Pos(1, 4)),
      MoveBuild(0, Pos(1, 3), Pos(2, 4)), MoveBuild(0, Pos(1, 3), Pos(0, 4)), MoveBuild(0, Pos(1, 3), Pos(0, 3)),
      MoveBuild(1, Pos(2, 0), Pos(3, 0)), MoveBuild(1, Pos(2, 0), Pos(2, 1)), MoveBuild(1, Pos(2, 0), Pos(3, 1)),
      MoveBuild(1, Pos(2, 0), Pos(1, 1)), MoveBuild(1, Pos(2, 0), Pos(1, 0)), MoveBuild(1, Pos(1, 1), Pos(2, 1)),
      MoveBuild(1, Pos(1, 1), Pos(1, 0)), MoveBuild(1, Pos(1, 1), Pos(2, 0)), MoveBuild(1, Pos(1, 1), Pos(1, 2)),
      MoveBuild(1, Pos(1, 1), Pos(2, 2)), MoveBuild(1, Pos(1, 1), Pos(0, 2)), MoveBuild(1, Pos(1, 1), Pos(0, 1)),
      MoveBuild(1, Pos(2, 1), Pos(3, 1)), MoveBuild(1, Pos(2, 1), Pos(2, 0)), MoveBuild(1, Pos(2, 1), Pos(3, 0)),
      MoveBuild(1, Pos(2, 1), Pos(1, 0)), MoveBuild(1, Pos(2, 1), Pos(2, 2)), MoveBuild(1, Pos(2, 1), Pos(3, 2)),
      MoveBuild(1, Pos(2, 1), Pos(1, 2)), MoveBuild(1, Pos(2, 1), Pos(1, 1)), MoveBuild(1, Pos(0, 1), Pos(1, 1)),
      MoveBuild(1, Pos(0, 1), Pos(1, 0)), MoveBuild(1, Pos(0, 1), Pos(0, 2)), MoveBuild(1, Pos(0, 1), Pos(1, 2))),
    nextPlayer = true)


  it should "restrictOppoScope" in {
    val scope = WarFogAnalysis.restrictOppoScope(meFirstState, null, null, null)

    scope.nonEmpty should be(true)

    scope.forall(set => set.contains(Pos(0, 0)) &&
      set.exists(pos => meFirstState.units.take(2).forall(_.distance(pos) > 1))) should be(true)

    scope.forall(set => set.size == 2 && set.take(1) != set.takeRight(1))
  }

  it should "retrict oppo scope with history event" in {

  }

}
