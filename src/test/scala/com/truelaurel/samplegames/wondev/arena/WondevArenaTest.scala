package com.truelaurel.samplegames.wondev.arena

import com.truelaurel.math.geometry.Pos
import com.truelaurel.samplegames.wondev.domain.{MoveBuild, PushBuild, WondevState}
import org.scalatest.{FlatSpec, Matchers}

/**
  * data copied from
  * mapIndex=0
  * seed=168724840
  */
class WondevArenaTest extends FlatSpec with Matchers {

  behavior of "WondevArenaTest"

  it should "nextLegalActions 1" in {
    val meFirstState = WondevState(5,
      Map(
        Pos(0, 2) -> 0, Pos(0, 0) -> 0, Pos(4, 0) -> 0, Pos(3, 4) -> 0, Pos(3, 1) -> 0,
        Pos(4, 1) -> 0, Pos(2, 0) -> 0, Pos(0, 3) -> 0, Pos(4, 4) -> 0, Pos(3, 0) -> 0,
        Pos(1, 1) -> 0, Pos(1, 4) -> 0, Pos(0, 4) -> 0, Pos(3, 2) -> 0, Pos(1, 3) -> 0,
        Pos(2, 2) -> 0, Pos(4, 2) -> 0, Pos(2, 4) -> 0, Pos(0, 1) -> 0, Pos(3, 3) -> 0,
        Pos(2, 3) -> 0, Pos(1, 2) -> 0, Pos(2, 1) -> 0, Pos(4, 3) -> 0, Pos(1, 0) -> 0),
      List(Pos(0, 4), Pos(1, 0), Pos(0, 0), Pos(-1, -1)),
      List(
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
        MoveBuild(1, Pos(0, 1), Pos(1, 0)), MoveBuild(1, Pos(0, 1), Pos(0, 2)), MoveBuild(1, Pos(0, 1), Pos(1, 2))), true)

    val expected = meFirstState.legalActions.toSet
    val actual = WondevArena.nextLegalActions(meFirstState).toSet
    actual should be(expected)
  }


  it should "nextLegalActions 2" in {
    val meFirstState = WondevState(5,
      Map(
        Pos(0, 2) -> 0, Pos(0, 0) -> 0, Pos(4, 0) -> 0, Pos(3, 4) -> 1, Pos(3, 1) -> 0,
        Pos(4, 1) -> 0, Pos(2, 0) -> 0, Pos(0, 3) -> 1, Pos(4, 4) -> 0, Pos(3, 0) -> 0,
        Pos(1, 1) -> 0, Pos(1, 4) -> 0, Pos(0, 4) -> 0, Pos(3, 2) -> 0, Pos(1, 3) -> 0,
        Pos(2, 2) -> 0, Pos(4, 2) -> 0, Pos(2, 4) -> 1, Pos(0, 1) -> 0, Pos(3, 3) -> 1,
        Pos(2, 3) -> 1, Pos(1, 2) -> 1, Pos(2, 1) -> 1, Pos(4, 3) -> 0, Pos(1, 0) -> 0),
      List(Pos(2, 3), Pos(1, 1), Pos(0, 2), Pos(-1, -1)),
      List(
        MoveBuild(0, Pos(3, 3), Pos(4, 3)), MoveBuild(0, Pos(3, 3), Pos(3, 2)), MoveBuild(0, Pos(3, 3), Pos(4, 2)),
        MoveBuild(0, Pos(3, 3), Pos(2, 2)), MoveBuild(0, Pos(3, 3), Pos(3, 4)), MoveBuild(0, Pos(3, 3), Pos(4, 4)),
        MoveBuild(0, Pos(3, 3), Pos(2, 4)), MoveBuild(0, Pos(3, 3), Pos(2, 3)), MoveBuild(0, Pos(2, 2), Pos(3, 2)),
        MoveBuild(0, Pos(2, 2), Pos(2, 1)), MoveBuild(0, Pos(2, 2), Pos(3, 1)), MoveBuild(0, Pos(2, 2), Pos(2, 3)),
        MoveBuild(0, Pos(2, 2), Pos(3, 3)), MoveBuild(0, Pos(2, 2), Pos(1, 3)), MoveBuild(0, Pos(2, 2), Pos(1, 2)),
        MoveBuild(0, Pos(3, 2), Pos(4, 2)), MoveBuild(0, Pos(3, 2), Pos(3, 1)), MoveBuild(0, Pos(3, 2), Pos(4, 1)),
        MoveBuild(0, Pos(3, 2), Pos(2, 1)), MoveBuild(0, Pos(3, 2), Pos(3, 3)), MoveBuild(0, Pos(3, 2), Pos(4, 3)),
        MoveBuild(0, Pos(3, 2), Pos(2, 3)), MoveBuild(0, Pos(3, 2), Pos(2, 2)), MoveBuild(0, Pos(1, 2), Pos(2, 2)),
        MoveBuild(0, Pos(1, 2), Pos(2, 1)), MoveBuild(0, Pos(1, 2), Pos(0, 1)), MoveBuild(0, Pos(1, 2), Pos(1, 3)),
        MoveBuild(0, Pos(1, 2), Pos(2, 3)), MoveBuild(0, Pos(1, 2), Pos(0, 3)), MoveBuild(0, Pos(2, 4), Pos(3, 4)),
        MoveBuild(0, Pos(2, 4), Pos(2, 3)), MoveBuild(0, Pos(2, 4), Pos(3, 3)), MoveBuild(0, Pos(2, 4), Pos(1, 3)),
        MoveBuild(0, Pos(2, 4), Pos(1, 4)), MoveBuild(0, Pos(3, 4), Pos(4, 4)), MoveBuild(0, Pos(3, 4), Pos(3, 3)),
        MoveBuild(0, Pos(3, 4), Pos(4, 3)), MoveBuild(0, Pos(3, 4), Pos(2, 3)), MoveBuild(0, Pos(3, 4), Pos(2, 4)),
        MoveBuild(0, Pos(1, 4), Pos(2, 4)), MoveBuild(0, Pos(1, 4), Pos(1, 3)), MoveBuild(0, Pos(1, 4), Pos(2, 3)),
        MoveBuild(0, Pos(1, 4), Pos(0, 3)), MoveBuild(0, Pos(1, 4), Pos(0, 4)), MoveBuild(0, Pos(1, 3), Pos(2, 3)),
        MoveBuild(0, Pos(1, 3), Pos(1, 2)), MoveBuild(0, Pos(1, 3), Pos(2, 2)), MoveBuild(0, Pos(1, 3), Pos(1, 4)),
        MoveBuild(0, Pos(1, 3), Pos(2, 4)), MoveBuild(0, Pos(1, 3), Pos(0, 4)), MoveBuild(0, Pos(1, 3), Pos(0, 3)),
        MoveBuild(1, Pos(2, 1), Pos(3, 1)), MoveBuild(1, Pos(2, 1), Pos(2, 0)), MoveBuild(1, Pos(2, 1), Pos(3, 0)),
        MoveBuild(1, Pos(2, 1), Pos(1, 0)), MoveBuild(1, Pos(2, 1), Pos(2, 2)), MoveBuild(1, Pos(2, 1), Pos(3, 2)),
        MoveBuild(1, Pos(2, 1), Pos(1, 2)), MoveBuild(1, Pos(2, 1), Pos(1, 1)), MoveBuild(1, Pos(1, 0), Pos(2, 0)),
        MoveBuild(1, Pos(1, 0), Pos(1, 1)), MoveBuild(1, Pos(1, 0), Pos(2, 1)), MoveBuild(1, Pos(1, 0), Pos(0, 1)),
        MoveBuild(1, Pos(1, 0), Pos(0, 0)), MoveBuild(1, Pos(2, 0), Pos(3, 0)), MoveBuild(1, Pos(2, 0), Pos(2, 1)),
        MoveBuild(1, Pos(2, 0), Pos(3, 1)), MoveBuild(1, Pos(2, 0), Pos(1, 1)), MoveBuild(1, Pos(2, 0), Pos(1, 0)),
        MoveBuild(1, Pos(0, 0), Pos(1, 0)), MoveBuild(1, Pos(0, 0), Pos(0, 1)), MoveBuild(1, Pos(0, 0), Pos(1, 1)),
        MoveBuild(1, Pos(1, 2), Pos(2, 2)), MoveBuild(1, Pos(1, 2), Pos(1, 1)), MoveBuild(1, Pos(1, 2), Pos(2, 1)),
        MoveBuild(1, Pos(1, 2), Pos(0, 1)), MoveBuild(1, Pos(1, 2), Pos(1, 3)), MoveBuild(1, Pos(1, 2), Pos(0, 3)),
        MoveBuild(1, Pos(2, 2), Pos(3, 2)), MoveBuild(1, Pos(2, 2), Pos(2, 1)), MoveBuild(1, Pos(2, 2), Pos(3, 1)),
        MoveBuild(1, Pos(2, 2), Pos(1, 1)), MoveBuild(1, Pos(2, 2), Pos(3, 3)), MoveBuild(1, Pos(2, 2), Pos(1, 3)),
        MoveBuild(1, Pos(2, 2), Pos(1, 2)), MoveBuild(1, Pos(0, 1), Pos(1, 1)), MoveBuild(1, Pos(0, 1), Pos(0, 0)),
        MoveBuild(1, Pos(0, 1), Pos(1, 0)), MoveBuild(1, Pos(0, 1), Pos(1, 2)), PushBuild(1, Pos(0, 2), Pos(0, 3))),
      true)

    val expected = meFirstState.legalActions.toSet
    val actual = WondevArena.nextLegalActions(meFirstState).toSet
    actual should be(expected)

  }


}
