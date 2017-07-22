package com.truelaurel.samplegames.wondev.analysis

import com.truelaurel.math.geometry.Pos
import com.truelaurel.samplegames.wondev.domain.{MoveBuild, PushBuild, WondevState}
import org.scalatest.{FlatSpec, Matchers}

/**
  * Created by hwang on 16/07/2017.
  */
class WarFogAnalysisTest extends FlatSpec with Matchers {

  behavior of "WarFogAnalysisTest"


  it should "restrict oppo scope without any history event" in {
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

    val scope = WarFogAnalysis.restrictOppoScope(meFirstState, null, null, null)

    scope.nonEmpty should be(true)

    scope.forall(set => set.contains(Pos(0, 0)) &&
      set.exists(pos => meFirstState.units.take(2).forall(_.distance(pos) > 1))) should be(true)

    scope.forall(set => set.size == 2 && set.take(1) != set.takeRight(1))
  }

  it should "restrict oppo scope with history event" in {

    val observed = WondevState(5,
      Map(
        Pos(0, 2) -> 0, Pos(0, 0) -> 0, Pos(4, 0) -> 0, Pos(3, 4) -> 1, Pos(3, 1) -> 0, Pos(4, 1) -> 0,
        Pos(2, 0) -> 0, Pos(0, 3) -> 0, Pos(4, 4) -> 0, Pos(3, 0) -> 0, Pos(1, 1) -> 0, Pos(1, 4) -> 0,
        Pos(0, 4) -> 0, Pos(3, 2) -> 0, Pos(1, 3) -> 0, Pos(2, 2) -> 0, Pos(4, 2) -> 0, Pos(2, 4) -> 0,
        Pos(0, 1) -> 0, Pos(3, 3) -> 0, Pos(2, 3) -> 0, Pos(1, 2) -> 0, Pos(2, 1) -> 1, Pos(4, 3) -> 0,
        Pos(1, 0) -> 0),
      List(
        Pos(0, 4), Pos(1, 1), Pos(0, 0), Pos(-1, -1)),
      List(
        MoveBuild(0, Pos(1, 4), Pos(2, 4)),
        MoveBuild(0, Pos(1, 4), Pos(1, 3)), MoveBuild(0, Pos(1, 4), Pos(2, 3)), MoveBuild(0, Pos(1, 4), Pos(0, 3)),
        MoveBuild(0, Pos(1, 4), Pos(0, 4)), MoveBuild(0, Pos(0, 3), Pos(1, 3)), MoveBuild(0, Pos(0, 3), Pos(0, 2)),
        MoveBuild(0, Pos(0, 3), Pos(1, 2)), MoveBuild(0, Pos(0, 3), Pos(0, 4)), MoveBuild(0, Pos(0, 3), Pos(1, 4)),
        MoveBuild(0, Pos(1, 3), Pos(2, 3)), MoveBuild(0, Pos(1, 3), Pos(1, 2)), MoveBuild(0, Pos(1, 3), Pos(2, 2)),
        MoveBuild(0, Pos(1, 3), Pos(0, 2)), MoveBuild(0, Pos(1, 3), Pos(1, 4)), MoveBuild(0, Pos(1, 3), Pos(2, 4)),
        MoveBuild(0, Pos(1, 3), Pos(0, 4)), MoveBuild(0, Pos(1, 3), Pos(0, 3)), MoveBuild(1, Pos(2, 1), Pos(3, 1)),
        MoveBuild(1, Pos(2, 1), Pos(2, 0)), MoveBuild(1, Pos(2, 1), Pos(3, 0)), MoveBuild(1, Pos(2, 1), Pos(1, 0)),
        MoveBuild(1, Pos(2, 1), Pos(2, 2)), MoveBuild(1, Pos(2, 1), Pos(3, 2)), MoveBuild(1, Pos(2, 1), Pos(1, 2)),
        MoveBuild(1, Pos(2, 1), Pos(1, 1)), MoveBuild(1, Pos(1, 0), Pos(2, 0)), MoveBuild(1, Pos(1, 0), Pos(1, 1)),
        MoveBuild(1, Pos(1, 0), Pos(2, 1)), MoveBuild(1, Pos(1, 0), Pos(0, 1)), MoveBuild(1, Pos(2, 0), Pos(3, 0)),
        MoveBuild(1, Pos(2, 0), Pos(2, 1)), MoveBuild(1, Pos(2, 0), Pos(3, 1)), MoveBuild(1, Pos(2, 0), Pos(1, 1)),
        MoveBuild(1, Pos(2, 0), Pos(1, 0)), MoveBuild(1, Pos(1, 2), Pos(2, 2)), MoveBuild(1, Pos(1, 2), Pos(1, 1)),
        MoveBuild(1, Pos(1, 2), Pos(2, 1)), MoveBuild(1, Pos(1, 2), Pos(0, 1)), MoveBuild(1, Pos(1, 2), Pos(1, 3)),
        MoveBuild(1, Pos(1, 2), Pos(2, 3)), MoveBuild(1, Pos(1, 2), Pos(0, 3)), MoveBuild(1, Pos(1, 2), Pos(0, 2)),
        MoveBuild(1, Pos(2, 2), Pos(3, 2)), MoveBuild(1, Pos(2, 2), Pos(2, 1)), MoveBuild(1, Pos(2, 2), Pos(3, 1)),
        MoveBuild(1, Pos(2, 2), Pos(1, 1)), MoveBuild(1, Pos(2, 2), Pos(2, 3)), MoveBuild(1, Pos(2, 2), Pos(3, 3)),
        MoveBuild(1, Pos(2, 2), Pos(1, 3)), MoveBuild(1, Pos(2, 2), Pos(1, 2)), MoveBuild(1, Pos(0, 2), Pos(1, 2)),
        MoveBuild(1, Pos(0, 2), Pos(0, 1)), MoveBuild(1, Pos(0, 2), Pos(1, 1)), MoveBuild(1, Pos(0, 2), Pos(0, 3)),
        MoveBuild(1, Pos(0, 2), Pos(1, 3)), MoveBuild(1, Pos(0, 1), Pos(1, 1)), MoveBuild(1, Pos(0, 1), Pos(1, 0)),
        MoveBuild(1, Pos(0, 1), Pos(0, 2)), MoveBuild(1, Pos(0, 1), Pos(1, 2))),
      true)

    val previousAction = MoveBuild(1, Pos(1, 1), Pos(2, 1))

    val previousOppoScope = Set(
      Set(Pos(0, 2), Pos(0, 0)), Set(Pos(0, 0), Pos(4, 4)), Set(Pos(0, 0), Pos(3, 3)),
      Set(Pos(0, 0), Pos(4, 0)), Set(Pos(0, 0), Pos(2, 2)), Set(Pos(0, 0), Pos(3, 1)),
      Set(Pos(0, 0), Pos(1, 2)), Set(Pos(0, 0), Pos(2, 4)), Set(Pos(0, 0), Pos(3, 4)),
      Set(Pos(0, 0), Pos(4, 1)), Set(Pos(0, 0), Pos(4, 3)), Set(Pos(0, 0), Pos(4, 2)),
      Set(Pos(0, 0), Pos(3, 0)), Set(Pos(0, 0), Pos(3, 2)), Set(Pos(0, 0), Pos(2, 3)))

    val previousState = WondevState(5,
      Map(
        Pos(0, 2) -> 0, Pos(0, 0) -> 0, Pos(4, 0) -> 0, Pos(3, 4) -> 0, Pos(3, 1) -> 0, Pos(4, 1) -> 0, Pos(2, 0) -> 0,
        Pos(0, 3) -> 0, Pos(4, 4) -> 0, Pos(3, 0) -> 0, Pos(1, 1) -> 0, Pos(1, 4) -> 0, Pos(0, 4) -> 0, Pos(3, 2) -> 0,
        Pos(1, 3) -> 0, Pos(2, 2) -> 0, Pos(4, 2) -> 0, Pos(2, 4) -> 0, Pos(0, 1) -> 0, Pos(3, 3) -> 0, Pos(2, 3) -> 0,
        Pos(1, 2) -> 0, Pos(2, 1) -> 0, Pos(4, 3) -> 0, Pos(1, 0) -> 0),
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
        MoveBuild(1, Pos(0, 1), Pos(1, 0)), MoveBuild(1, Pos(0, 1), Pos(0, 2)), MoveBuild(1, Pos(0, 1), Pos(1, 2))),
      true)

    val restricted = WarFogAnalysis.restrictOppoScope(observed, previousState, previousAction, previousOppoScope)

    restricted.nonEmpty should be(true)

    restricted.forall(set =>
      set.contains(Pos(0, 0)) &&
        set.exists(pos => observed.units.take(2).forall(_.distance(pos) > 1)) &&
        set.exists(pos => pos.distance(Pos(3, 4)) == 1)
    ) should be(true)

    restricted.forall(set => set.size == 2 && set.take(1) != set.takeRight(1))

  }

  it should "restrict oppo scope with history event 2" in {

    val observed = WondevState(5,
      Map(
        Pos(0, 2) -> 0, Pos(0, 0) -> 0, Pos(4, 0) -> 0, Pos(3, 4) -> 1, Pos(3, 1) -> 0, Pos(4, 1) -> 0, Pos(2, 0) -> 0,
        Pos(0, 3) -> 0, Pos(4, 4) -> 0, Pos(3, 0) -> 0, Pos(1, 1) -> 0, Pos(1, 4) -> 0, Pos(0, 4) -> 0, Pos(3, 2) -> 0,
        Pos(1, 3) -> 0, Pos(2, 2) -> 0, Pos(4, 2) -> 0, Pos(2, 4) -> 1, Pos(0, 1) -> 0, Pos(3, 3) -> 0, Pos(2, 3) -> 1,
        Pos(1, 2) -> 0, Pos(2, 1) -> 1, Pos(4, 3) -> 0, Pos(1, 0) -> 0), List(Pos(1, 3), Pos(1, 1), Pos(0, 0), Pos(-1, -1)),
      List(
        MoveBuild(0, Pos(2, 3), Pos(3, 3)), MoveBuild(0, Pos(2, 3), Pos(2, 2)), MoveBuild(0, Pos(2, 3), Pos(3, 2)),
        MoveBuild(0, Pos(2, 3), Pos(1, 2)), MoveBuild(0, Pos(2, 3), Pos(2, 4)), MoveBuild(0, Pos(2, 3), Pos(3, 4)),
        MoveBuild(0, Pos(2, 3), Pos(1, 4)), MoveBuild(0, Pos(2, 3), Pos(1, 3)), MoveBuild(0, Pos(1, 2), Pos(2, 2)),
        MoveBuild(0, Pos(1, 2), Pos(2, 1)), MoveBuild(0, Pos(1, 2), Pos(0, 1)), MoveBuild(0, Pos(1, 2), Pos(1, 3)),
        MoveBuild(0, Pos(1, 2), Pos(2, 3)), MoveBuild(0, Pos(1, 2), Pos(0, 3)), MoveBuild(0, Pos(1, 2), Pos(0, 2)),
        MoveBuild(0, Pos(2, 2), Pos(3, 2)), MoveBuild(0, Pos(2, 2), Pos(2, 1)), MoveBuild(0, Pos(2, 2), Pos(3, 1)),
        MoveBuild(0, Pos(2, 2), Pos(2, 3)), MoveBuild(0, Pos(2, 2), Pos(3, 3)), MoveBuild(0, Pos(2, 2), Pos(1, 3)),
        MoveBuild(0, Pos(2, 2), Pos(1, 2)), MoveBuild(0, Pos(0, 2), Pos(1, 2)), MoveBuild(0, Pos(0, 2), Pos(0, 1)),
        MoveBuild(0, Pos(0, 2), Pos(0, 3)), MoveBuild(0, Pos(0, 2), Pos(1, 3)), MoveBuild(0, Pos(1, 4), Pos(2, 4)),
        MoveBuild(0, Pos(1, 4), Pos(1, 3)), MoveBuild(0, Pos(1, 4), Pos(2, 3)), MoveBuild(0, Pos(1, 4), Pos(0, 3)),
        MoveBuild(0, Pos(1, 4), Pos(0, 4)), MoveBuild(0, Pos(2, 4), Pos(3, 4)), MoveBuild(0, Pos(2, 4), Pos(2, 3)),
        MoveBuild(0, Pos(2, 4), Pos(3, 3)), MoveBuild(0, Pos(2, 4), Pos(1, 3)), MoveBuild(0, Pos(2, 4), Pos(1, 4)),
        MoveBuild(0, Pos(0, 4), Pos(1, 4)), MoveBuild(0, Pos(0, 4), Pos(0, 3)), MoveBuild(0, Pos(0, 4), Pos(1, 3)),
        MoveBuild(0, Pos(0, 3), Pos(1, 3)), MoveBuild(0, Pos(0, 3), Pos(0, 2)), MoveBuild(0, Pos(0, 3), Pos(1, 2)),
        MoveBuild(0, Pos(0, 3), Pos(0, 4)), MoveBuild(0, Pos(0, 3), Pos(1, 4)), MoveBuild(1, Pos(2, 1), Pos(3, 1)),
        MoveBuild(1, Pos(2, 1), Pos(2, 0)), MoveBuild(1, Pos(2, 1), Pos(3, 0)), MoveBuild(1, Pos(2, 1), Pos(1, 0)),
        MoveBuild(1, Pos(2, 1), Pos(2, 2)), MoveBuild(1, Pos(2, 1), Pos(3, 2)), MoveBuild(1, Pos(2, 1), Pos(1, 2)),
        MoveBuild(1, Pos(2, 1), Pos(1, 1)), MoveBuild(1, Pos(1, 0), Pos(2, 0)), MoveBuild(1, Pos(1, 0), Pos(1, 1)),
        MoveBuild(1, Pos(1, 0), Pos(2, 1)), MoveBuild(1, Pos(1, 0), Pos(0, 1)), MoveBuild(1, Pos(2, 0), Pos(3, 0)),
        MoveBuild(1, Pos(2, 0), Pos(2, 1)), MoveBuild(1, Pos(2, 0), Pos(3, 1)), MoveBuild(1, Pos(2, 0), Pos(1, 1)),
        MoveBuild(1, Pos(2, 0), Pos(1, 0)), MoveBuild(1, Pos(1, 2), Pos(2, 2)), MoveBuild(1, Pos(1, 2), Pos(1, 1)),
        MoveBuild(1, Pos(1, 2), Pos(2, 1)), MoveBuild(1, Pos(1, 2), Pos(0, 1)), MoveBuild(1, Pos(1, 2), Pos(2, 3)),
        MoveBuild(1, Pos(1, 2), Pos(0, 3)), MoveBuild(1, Pos(1, 2), Pos(0, 2)), MoveBuild(1, Pos(2, 2), Pos(3, 2)),
        MoveBuild(1, Pos(2, 2), Pos(2, 1)), MoveBuild(1, Pos(2, 2), Pos(3, 1)), MoveBuild(1, Pos(2, 2), Pos(1, 1)),
        MoveBuild(1, Pos(2, 2), Pos(2, 3)), MoveBuild(1, Pos(2, 2), Pos(3, 3)), MoveBuild(1, Pos(2, 2), Pos(1, 2)),
        MoveBuild(1, Pos(0, 2), Pos(1, 2)), MoveBuild(1, Pos(0, 2), Pos(0, 1)), MoveBuild(1, Pos(0, 2), Pos(1, 1)),
        MoveBuild(1, Pos(0, 2), Pos(0, 3)), MoveBuild(1, Pos(0, 1), Pos(1, 1)), MoveBuild(1, Pos(0, 1), Pos(1, 0)),
        MoveBuild(1, Pos(0, 1), Pos(0, 2)), MoveBuild(1, Pos(0, 1), Pos(1, 2))), true)

    val previousAction = MoveBuild(0, Pos(1, 3), Pos(2, 3))

    val previousOppoScope = Set(Set(Pos(0, 0), Pos(4, 4)), Set(Pos(0, 0), Pos(3, 3)), Set(Pos(0, 0), Pos(2, 4)),
      Set(Pos(0, 0), Pos(4, 3)), Set(Pos(0, 0), Pos(2, 3)))

    val previousState = WondevState(5,
      Map(
        Pos(0, 2) -> 0, Pos(0, 0) -> 0, Pos(4, 0) -> 0, Pos(3, 4) -> 1, Pos(3, 1) -> 0, Pos(4, 1) -> 0,
        Pos(2, 0) -> 0, Pos(0, 3) -> 0, Pos(4, 4) -> 0, Pos(3, 0) -> 0, Pos(1, 1) -> 0, Pos(1, 4) -> 0,
        Pos(0, 4) -> 0, Pos(3, 2) -> 0, Pos(1, 3) -> 0, Pos(2, 2) -> 0, Pos(4, 2) -> 0, Pos(2, 4) -> 0,
        Pos(0, 1) -> 0, Pos(3, 3) -> 0, Pos(2, 3) -> 0, Pos(1, 2) -> 0, Pos(2, 1) -> 1, Pos(4, 3) -> 0,
        Pos(1, 0) -> 0),
      List(
        Pos(0, 4), Pos(1, 1), Pos(0, 0), Pos(-1, -1)),
      List(
        MoveBuild(0, Pos(1, 4), Pos(2, 4)),
        MoveBuild(0, Pos(1, 4), Pos(1, 3)), MoveBuild(0, Pos(1, 4), Pos(2, 3)), MoveBuild(0, Pos(1, 4), Pos(0, 3)),
        MoveBuild(0, Pos(1, 4), Pos(0, 4)), MoveBuild(0, Pos(0, 3), Pos(1, 3)), MoveBuild(0, Pos(0, 3), Pos(0, 2)),
        MoveBuild(0, Pos(0, 3), Pos(1, 2)), MoveBuild(0, Pos(0, 3), Pos(0, 4)), MoveBuild(0, Pos(0, 3), Pos(1, 4)),
        MoveBuild(0, Pos(1, 3), Pos(2, 3)), MoveBuild(0, Pos(1, 3), Pos(1, 2)), MoveBuild(0, Pos(1, 3), Pos(2, 2)),
        MoveBuild(0, Pos(1, 3), Pos(0, 2)), MoveBuild(0, Pos(1, 3), Pos(1, 4)), MoveBuild(0, Pos(1, 3), Pos(2, 4)),
        MoveBuild(0, Pos(1, 3), Pos(0, 4)), MoveBuild(0, Pos(1, 3), Pos(0, 3)), MoveBuild(1, Pos(2, 1), Pos(3, 1)),
        MoveBuild(1, Pos(2, 1), Pos(2, 0)), MoveBuild(1, Pos(2, 1), Pos(3, 0)), MoveBuild(1, Pos(2, 1), Pos(1, 0)),
        MoveBuild(1, Pos(2, 1), Pos(2, 2)), MoveBuild(1, Pos(2, 1), Pos(3, 2)), MoveBuild(1, Pos(2, 1), Pos(1, 2)),
        MoveBuild(1, Pos(2, 1), Pos(1, 1)), MoveBuild(1, Pos(1, 0), Pos(2, 0)), MoveBuild(1, Pos(1, 0), Pos(1, 1)),
        MoveBuild(1, Pos(1, 0), Pos(2, 1)), MoveBuild(1, Pos(1, 0), Pos(0, 1)), MoveBuild(1, Pos(2, 0), Pos(3, 0)),
        MoveBuild(1, Pos(2, 0), Pos(2, 1)), MoveBuild(1, Pos(2, 0), Pos(3, 1)), MoveBuild(1, Pos(2, 0), Pos(1, 1)),
        MoveBuild(1, Pos(2, 0), Pos(1, 0)), MoveBuild(1, Pos(1, 2), Pos(2, 2)), MoveBuild(1, Pos(1, 2), Pos(1, 1)),
        MoveBuild(1, Pos(1, 2), Pos(2, 1)), MoveBuild(1, Pos(1, 2), Pos(0, 1)), MoveBuild(1, Pos(1, 2), Pos(1, 3)),
        MoveBuild(1, Pos(1, 2), Pos(2, 3)), MoveBuild(1, Pos(1, 2), Pos(0, 3)), MoveBuild(1, Pos(1, 2), Pos(0, 2)),
        MoveBuild(1, Pos(2, 2), Pos(3, 2)), MoveBuild(1, Pos(2, 2), Pos(2, 1)), MoveBuild(1, Pos(2, 2), Pos(3, 1)),
        MoveBuild(1, Pos(2, 2), Pos(1, 1)), MoveBuild(1, Pos(2, 2), Pos(2, 3)), MoveBuild(1, Pos(2, 2), Pos(3, 3)),
        MoveBuild(1, Pos(2, 2), Pos(1, 3)), MoveBuild(1, Pos(2, 2), Pos(1, 2)), MoveBuild(1, Pos(0, 2), Pos(1, 2)),
        MoveBuild(1, Pos(0, 2), Pos(0, 1)), MoveBuild(1, Pos(0, 2), Pos(1, 1)), MoveBuild(1, Pos(0, 2), Pos(0, 3)),
        MoveBuild(1, Pos(0, 2), Pos(1, 3)), MoveBuild(1, Pos(0, 1), Pos(1, 1)), MoveBuild(1, Pos(0, 1), Pos(1, 0)),
        MoveBuild(1, Pos(0, 1), Pos(0, 2)), MoveBuild(1, Pos(0, 1), Pos(1, 2))),
      true)

    val restricted = WarFogAnalysis.restrictOppoScope(observed, previousState, previousAction, previousOppoScope)

    restricted.size should be(2)

    restricted.forall(set =>
      set.contains(Pos(0, 0)) &&
        set.exists(pos => observed.units.take(2).forall(_.distance(pos) > 1)) &&
        set.exists(pos => pos.distance(Pos(2, 4)) == 1)
    ) should be(true)

    restricted.forall(set => set.size == 2 && set.take(1) != set.takeRight(1))

  }

  it should "restrict oppo scope with history event 3" in {

    val observed = WondevState(5,
      Map(
        Pos(0, 2) -> 0, Pos(0, 0) -> 0, Pos(4, 0) -> 0, Pos(3, 4) -> 1, Pos(3, 1) -> 0, Pos(4, 1) -> 0, Pos(2, 0) -> 0,
        Pos(0, 3) -> 0, Pos(4, 4) -> 0, Pos(3, 0) -> 0, Pos(1, 1) -> 0, Pos(1, 4) -> 0, Pos(0, 4) -> 0, Pos(3, 2) -> 0,
        Pos(1, 3) -> 0, Pos(2, 2) -> 0, Pos(4, 2) -> 0, Pos(2, 4) -> 1, Pos(0, 1) -> 0, Pos(3, 3) -> 0, Pos(2, 3) -> 1,
        Pos(1, 2) -> 1, Pos(2, 1) -> 1, Pos(4, 3) -> 0, Pos(1, 0) -> 0),
      List(Pos(2, 3), Pos(1, 1), Pos(0, 1), Pos(3, 3)),
      List(
        MoveBuild(0, Pos(2, 2), Pos(3, 2)), MoveBuild(0, Pos(2, 2), Pos(2, 1)), MoveBuild(0, Pos(2, 2), Pos(3, 1)),
        MoveBuild(0, Pos(2, 2), Pos(2, 3)), MoveBuild(0, Pos(2, 2), Pos(1, 3)), MoveBuild(0, Pos(2, 2), Pos(1, 2)),
        MoveBuild(0, Pos(3, 2), Pos(4, 2)), MoveBuild(0, Pos(3, 2), Pos(3, 1)), MoveBuild(0, Pos(3, 2), Pos(4, 1)),
        MoveBuild(0, Pos(3, 2), Pos(2, 1)), MoveBuild(0, Pos(3, 2), Pos(4, 3)), MoveBuild(0, Pos(3, 2), Pos(2, 3)),
        MoveBuild(0, Pos(3, 2), Pos(2, 2)), MoveBuild(0, Pos(1, 2), Pos(2, 2)), MoveBuild(0, Pos(1, 2), Pos(2, 1)),
        MoveBuild(0, Pos(1, 2), Pos(1, 3)), MoveBuild(0, Pos(1, 2), Pos(2, 3)), MoveBuild(0, Pos(1, 2), Pos(0, 3)),
        MoveBuild(0, Pos(1, 2), Pos(0, 2)), MoveBuild(0, Pos(2, 4), Pos(3, 4)), MoveBuild(0, Pos(2, 4), Pos(2, 3)),
        MoveBuild(0, Pos(2, 4), Pos(1, 3)), MoveBuild(0, Pos(2, 4), Pos(1, 4)), MoveBuild(0, Pos(3, 4), Pos(4, 4)),
        MoveBuild(0, Pos(3, 4), Pos(4, 3)), MoveBuild(0, Pos(3, 4), Pos(2, 3)), MoveBuild(0, Pos(3, 4), Pos(2, 4)),
        MoveBuild(0, Pos(1, 4), Pos(2, 4)), MoveBuild(0, Pos(1, 4), Pos(1, 3)), MoveBuild(0, Pos(1, 4), Pos(2, 3)),
        MoveBuild(0, Pos(1, 4), Pos(0, 3)), MoveBuild(0, Pos(1, 4), Pos(0, 4)), MoveBuild(0, Pos(1, 3), Pos(2, 3)),
        MoveBuild(0, Pos(1, 3), Pos(1, 2)), MoveBuild(0, Pos(1, 3), Pos(2, 2)), MoveBuild(0, Pos(1, 3), Pos(0, 2)),
        MoveBuild(0, Pos(1, 3), Pos(1, 4)), MoveBuild(0, Pos(1, 3), Pos(2, 4)), MoveBuild(0, Pos(1, 3), Pos(0, 4)),
        MoveBuild(0, Pos(1, 3), Pos(0, 3)), MoveBuild(1, Pos(2, 1), Pos(3, 1)), MoveBuild(1, Pos(2, 1), Pos(2, 0)),
        MoveBuild(1, Pos(2, 1), Pos(3, 0)), MoveBuild(1, Pos(2, 1), Pos(1, 0)), MoveBuild(1, Pos(2, 1), Pos(2, 2)),
        MoveBuild(1, Pos(2, 1), Pos(3, 2)), MoveBuild(1, Pos(2, 1), Pos(1, 2)), MoveBuild(1, Pos(2, 1), Pos(1, 1)),
        MoveBuild(1, Pos(1, 0), Pos(2, 0)), MoveBuild(1, Pos(1, 0), Pos(1, 1)), MoveBuild(1, Pos(1, 0), Pos(2, 1)),
        MoveBuild(1, Pos(1, 0), Pos(0, 0)), MoveBuild(1, Pos(2, 0), Pos(3, 0)), MoveBuild(1, Pos(2, 0), Pos(2, 1)),
        MoveBuild(1, Pos(2, 0), Pos(3, 1)), MoveBuild(1, Pos(2, 0), Pos(1, 1)), MoveBuild(1, Pos(2, 0), Pos(1, 0)),
        MoveBuild(1, Pos(0, 0), Pos(1, 0)), MoveBuild(1, Pos(0, 0), Pos(1, 1)), MoveBuild(1, Pos(1, 2), Pos(2, 2)),
        MoveBuild(1, Pos(1, 2), Pos(1, 1)), MoveBuild(1, Pos(1, 2), Pos(2, 1)), MoveBuild(1, Pos(1, 2), Pos(1, 3)),
        MoveBuild(1, Pos(1, 2), Pos(0, 3)), MoveBuild(1, Pos(1, 2), Pos(0, 2)), MoveBuild(1, Pos(2, 2), Pos(3, 2)),
        MoveBuild(1, Pos(2, 2), Pos(2, 1)), MoveBuild(1, Pos(2, 2), Pos(3, 1)), MoveBuild(1, Pos(2, 2), Pos(1, 1)),
        MoveBuild(1, Pos(2, 2), Pos(1, 3)), MoveBuild(1, Pos(2, 2), Pos(1, 2)), MoveBuild(1, Pos(0, 2), Pos(1, 2)),
        MoveBuild(1, Pos(0, 2), Pos(1, 1)), MoveBuild(1, Pos(0, 2), Pos(0, 3)), MoveBuild(1, Pos(0, 2), Pos(1, 3)),
        PushBuild(0, Pos(3, 3), Pos(4, 3)), PushBuild(0, Pos(3, 3), Pos(4, 2)), PushBuild(0, Pos(3, 3), Pos(4, 4))), true)

    val previousAction = MoveBuild(0, Pos(2, 3), Pos(3, 3))

    val previousOppoScope = Set(Set(Pos(0, 0), Pos(3, 4)), Set(Pos(0, 0), Pos(3, 3)))

    val previousState = WondevState(5,
      Map(
        Pos(0, 2) -> 0, Pos(0, 0) -> 0, Pos(4, 0) -> 0, Pos(3, 4) -> 1, Pos(3, 1) -> 0, Pos(4, 1) -> 0, Pos(2, 0) -> 0,
        Pos(0, 3) -> 0, Pos(4, 4) -> 0, Pos(3, 0) -> 0, Pos(1, 1) -> 0, Pos(1, 4) -> 0, Pos(0, 4) -> 0, Pos(3, 2) -> 0,
        Pos(1, 3) -> 0, Pos(2, 2) -> 0, Pos(4, 2) -> 0, Pos(2, 4) -> 1, Pos(0, 1) -> 0, Pos(3, 3) -> 0, Pos(2, 3) -> 1,
        Pos(1, 2) -> 0, Pos(2, 1) -> 1, Pos(4, 3) -> 0, Pos(1, 0) -> 0), List(Pos(1, 3), Pos(1, 1), Pos(0, 0), Pos(-1, -1)),
      List(
        MoveBuild(0, Pos(2, 3), Pos(3, 3)), MoveBuild(0, Pos(2, 3), Pos(2, 2)), MoveBuild(0, Pos(2, 3), Pos(3, 2)),
        MoveBuild(0, Pos(2, 3), Pos(1, 2)), MoveBuild(0, Pos(2, 3), Pos(2, 4)), MoveBuild(0, Pos(2, 3), Pos(3, 4)),
        MoveBuild(0, Pos(2, 3), Pos(1, 4)), MoveBuild(0, Pos(2, 3), Pos(1, 3)), MoveBuild(0, Pos(1, 2), Pos(2, 2)),
        MoveBuild(0, Pos(1, 2), Pos(2, 1)), MoveBuild(0, Pos(1, 2), Pos(0, 1)), MoveBuild(0, Pos(1, 2), Pos(1, 3)),
        MoveBuild(0, Pos(1, 2), Pos(2, 3)), MoveBuild(0, Pos(1, 2), Pos(0, 3)), MoveBuild(0, Pos(1, 2), Pos(0, 2)),
        MoveBuild(0, Pos(2, 2), Pos(3, 2)), MoveBuild(0, Pos(2, 2), Pos(2, 1)), MoveBuild(0, Pos(2, 2), Pos(3, 1)),
        MoveBuild(0, Pos(2, 2), Pos(2, 3)), MoveBuild(0, Pos(2, 2), Pos(3, 3)), MoveBuild(0, Pos(2, 2), Pos(1, 3)),
        MoveBuild(0, Pos(2, 2), Pos(1, 2)), MoveBuild(0, Pos(0, 2), Pos(1, 2)), MoveBuild(0, Pos(0, 2), Pos(0, 1)),
        MoveBuild(0, Pos(0, 2), Pos(0, 3)), MoveBuild(0, Pos(0, 2), Pos(1, 3)), MoveBuild(0, Pos(1, 4), Pos(2, 4)),
        MoveBuild(0, Pos(1, 4), Pos(1, 3)), MoveBuild(0, Pos(1, 4), Pos(2, 3)), MoveBuild(0, Pos(1, 4), Pos(0, 3)),
        MoveBuild(0, Pos(1, 4), Pos(0, 4)), MoveBuild(0, Pos(2, 4), Pos(3, 4)), MoveBuild(0, Pos(2, 4), Pos(2, 3)),
        MoveBuild(0, Pos(2, 4), Pos(3, 3)), MoveBuild(0, Pos(2, 4), Pos(1, 3)), MoveBuild(0, Pos(2, 4), Pos(1, 4)),
        MoveBuild(0, Pos(0, 4), Pos(1, 4)), MoveBuild(0, Pos(0, 4), Pos(0, 3)), MoveBuild(0, Pos(0, 4), Pos(1, 3)),
        MoveBuild(0, Pos(0, 3), Pos(1, 3)), MoveBuild(0, Pos(0, 3), Pos(0, 2)), MoveBuild(0, Pos(0, 3), Pos(1, 2)),
        MoveBuild(0, Pos(0, 3), Pos(0, 4)), MoveBuild(0, Pos(0, 3), Pos(1, 4)), MoveBuild(1, Pos(2, 1), Pos(3, 1)),
        MoveBuild(1, Pos(2, 1), Pos(2, 0)), MoveBuild(1, Pos(2, 1), Pos(3, 0)), MoveBuild(1, Pos(2, 1), Pos(1, 0)),
        MoveBuild(1, Pos(2, 1), Pos(2, 2)), MoveBuild(1, Pos(2, 1), Pos(3, 2)), MoveBuild(1, Pos(2, 1), Pos(1, 2)),
        MoveBuild(1, Pos(2, 1), Pos(1, 1)), MoveBuild(1, Pos(1, 0), Pos(2, 0)), MoveBuild(1, Pos(1, 0), Pos(1, 1)),
        MoveBuild(1, Pos(1, 0), Pos(2, 1)), MoveBuild(1, Pos(1, 0), Pos(0, 1)), MoveBuild(1, Pos(2, 0), Pos(3, 0)),
        MoveBuild(1, Pos(2, 0), Pos(2, 1)), MoveBuild(1, Pos(2, 0), Pos(3, 1)), MoveBuild(1, Pos(2, 0), Pos(1, 1)),
        MoveBuild(1, Pos(2, 0), Pos(1, 0)), MoveBuild(1, Pos(1, 2), Pos(2, 2)), MoveBuild(1, Pos(1, 2), Pos(1, 1)),
        MoveBuild(1, Pos(1, 2), Pos(2, 1)), MoveBuild(1, Pos(1, 2), Pos(0, 1)), MoveBuild(1, Pos(1, 2), Pos(2, 3)),
        MoveBuild(1, Pos(1, 2), Pos(0, 3)), MoveBuild(1, Pos(1, 2), Pos(0, 2)), MoveBuild(1, Pos(2, 2), Pos(3, 2)),
        MoveBuild(1, Pos(2, 2), Pos(2, 1)), MoveBuild(1, Pos(2, 2), Pos(3, 1)), MoveBuild(1, Pos(2, 2), Pos(1, 1)),
        MoveBuild(1, Pos(2, 2), Pos(2, 3)), MoveBuild(1, Pos(2, 2), Pos(3, 3)), MoveBuild(1, Pos(2, 2), Pos(1, 2)),
        MoveBuild(1, Pos(0, 2), Pos(1, 2)), MoveBuild(1, Pos(0, 2), Pos(0, 1)), MoveBuild(1, Pos(0, 2), Pos(1, 1)),
        MoveBuild(1, Pos(0, 2), Pos(0, 3)), MoveBuild(1, Pos(0, 1), Pos(1, 1)), MoveBuild(1, Pos(0, 1), Pos(1, 0)),
        MoveBuild(1, Pos(0, 1), Pos(0, 2)), MoveBuild(1, Pos(0, 1), Pos(1, 2))), true)

    val restricted = WarFogAnalysis.restrictOppoScope(observed, previousState, previousAction, previousOppoScope)

    restricted should be(Set(Set(Pos(0, 1), Pos(3, 3))))


  }

  it should "restrict oppo scope with history event 4" in {

    val observed = WondevState(5,Map(Pos(0,2) -> 0, Pos(0,0) -> 0, Pos(4,0) -> 0, Pos(3,4) -> 2, Pos(3,1) -> 0, Pos(4,1) -> 0, Pos(2,0) -> 0, Pos(0,3) -> 1, Pos(4,4) -> 0, Pos(3,0) -> 0, Pos(1,1) -> 0, Pos(1,4) -> 0, Pos(0,4) -> 0, Pos(3,2) -> 1, Pos(1,3) -> 0, Pos(2,2) -> 0, Pos(4,2) -> 0, Pos(2,4) -> 2, Pos(0,1) -> 0, Pos(3,3) -> 2, Pos(2,3) -> 1, Pos(1,2) -> 3, Pos(2,1) -> 1, Pos(4,3) -> 1, Pos(1,0) -> 0),List(Pos(3,3), Pos(1,1), Pos(0,1), Pos(4,4)),List(MoveBuild(0,Pos(4,3),Pos(4,2)), MoveBuild(0,Pos(4,3),Pos(3,2)), MoveBuild(0,Pos(4,3),Pos(3,4)), MoveBuild(0,Pos(4,3),Pos(3,3)), MoveBuild(0,Pos(3,2),Pos(4,2)), MoveBuild(0,Pos(3,2),Pos(3,1)), MoveBuild(0,Pos(3,2),Pos(4,1)), MoveBuild(0,Pos(3,2),Pos(2,1)), MoveBuild(0,Pos(3,2),Pos(3,3)), MoveBuild(0,Pos(3,2),Pos(4,3)), MoveBuild(0,Pos(3,2),Pos(2,3)), MoveBuild(0,Pos(3,2),Pos(2,2)), MoveBuild(0,Pos(4,2),Pos(4,1)), MoveBuild(0,Pos(4,2),Pos(3,1)), MoveBuild(0,Pos(4,2),Pos(4,3)), MoveBuild(0,Pos(4,2),Pos(3,3)), MoveBuild(0,Pos(4,2),Pos(3,2)), MoveBuild(0,Pos(2,2),Pos(3,2)), MoveBuild(0,Pos(2,2),Pos(2,1)), MoveBuild(0,Pos(2,2),Pos(3,1)), MoveBuild(0,Pos(2,2),Pos(2,3)), MoveBuild(0,Pos(2,2),Pos(3,3)), MoveBuild(0,Pos(2,2),Pos(1,3)), MoveBuild(0,Pos(2,2),Pos(1,2)), MoveBuild(0,Pos(3,4),Pos(3,3)), MoveBuild(0,Pos(3,4),Pos(4,3)), MoveBuild(0,Pos(3,4),Pos(2,3)), MoveBuild(0,Pos(3,4),Pos(2,4)), MoveBuild(0,Pos(2,4),Pos(3,4)), MoveBuild(0,Pos(2,4),Pos(2,3)), MoveBuild(0,Pos(2,4),Pos(3,3)), MoveBuild(0,Pos(2,4),Pos(1,3)), MoveBuild(0,Pos(2,4),Pos(1,4)), MoveBuild(0,Pos(2,3),Pos(3,3)), MoveBuild(0,Pos(2,3),Pos(2,2)), MoveBuild(0,Pos(2,3),Pos(3,2)), MoveBuild(0,Pos(2,3),Pos(1,2)), MoveBuild(0,Pos(2,3),Pos(2,4)), MoveBuild(0,Pos(2,3),Pos(3,4)), MoveBuild(0,Pos(2,3),Pos(1,4)), MoveBuild(0,Pos(2,3),Pos(1,3)), MoveBuild(1,Pos(2,1),Pos(3,1)), MoveBuild(1,Pos(2,1),Pos(2,0)), MoveBuild(1,Pos(2,1),Pos(3,0)), MoveBuild(1,Pos(2,1),Pos(1,0)), MoveBuild(1,Pos(2,1),Pos(2,2)), MoveBuild(1,Pos(2,1),Pos(3,2)), MoveBuild(1,Pos(2,1),Pos(1,2)), MoveBuild(1,Pos(2,1),Pos(1,1)), MoveBuild(1,Pos(1,0),Pos(2,0)), MoveBuild(1,Pos(1,0),Pos(1,1)), MoveBuild(1,Pos(1,0),Pos(2,1)), MoveBuild(1,Pos(1,0),Pos(0,0)), MoveBuild(1,Pos(2,0),Pos(3,0)), MoveBuild(1,Pos(2,0),Pos(2,1)), MoveBuild(1,Pos(2,0),Pos(3,1)), MoveBuild(1,Pos(2,0),Pos(1,1)), MoveBuild(1,Pos(2,0),Pos(1,0)), MoveBuild(1,Pos(0,0),Pos(1,0)), MoveBuild(1,Pos(0,0),Pos(1,1)), MoveBuild(1,Pos(2,2),Pos(3,2)), MoveBuild(1,Pos(2,2),Pos(2,1)), MoveBuild(1,Pos(2,2),Pos(3,1)), MoveBuild(1,Pos(2,2),Pos(1,1)), MoveBuild(1,Pos(2,2),Pos(2,3)), MoveBuild(1,Pos(2,2),Pos(1,3)), MoveBuild(1,Pos(2,2),Pos(1,2)), MoveBuild(1,Pos(0,2),Pos(1,2)), MoveBuild(1,Pos(0,2),Pos(1,1)), MoveBuild(1,Pos(0,2),Pos(0,3)), MoveBuild(1,Pos(0,2),Pos(1,3))),true)


    val previousAction = MoveBuild(0,Pos(3,3),Pos(4,3))

    val previousOppoScope = Set(Set(Pos(0,1), Pos(4,3)))

    val previousState = WondevState(5,Map(Pos(0,2) -> 0, Pos(0,0) -> 0, Pos(4,0) -> 0, Pos(3,4) -> 2, Pos(3,1) -> 0, Pos(4,1) -> 0, Pos(2,0) -> 0, Pos(0,3) -> 1, Pos(4,4) -> 0, Pos(3,0) -> 0, Pos(1,1) -> 0, Pos(1,4) -> 0, Pos(0,4) -> 0, Pos(3,2) -> 1, Pos(1,3) -> 0, Pos(2,2) -> 0, Pos(4,2) -> 0, Pos(2,4) -> 2, Pos(0,1) -> 0, Pos(3,3) -> 2, Pos(2,3) -> 1, Pos(1,2) -> 3, Pos(2,1) -> 1, Pos(4,3) -> 0, Pos(1,0) -> 0),List(Pos(2,3), Pos(1,1), Pos(0,1), Pos(-1,-1)),List(MoveBuild(0,Pos(3,3),Pos(4,3)), MoveBuild(0,Pos(3,3),Pos(3,2)), MoveBuild(0,Pos(3,3),Pos(4,2)), MoveBuild(0,Pos(3,3),Pos(2,2)), MoveBuild(0,Pos(3,3),Pos(3,4)), MoveBuild(0,Pos(3,3),Pos(4,4)), MoveBuild(0,Pos(3,3),Pos(2,4)), MoveBuild(0,Pos(3,3),Pos(2,3)), MoveBuild(0,Pos(2,2),Pos(3,2)), MoveBuild(0,Pos(2,2),Pos(2,1)), MoveBuild(0,Pos(2,2),Pos(3,1)), MoveBuild(0,Pos(2,2),Pos(2,3)), MoveBuild(0,Pos(2,2),Pos(3,3)), MoveBuild(0,Pos(2,2),Pos(1,3)), MoveBuild(0,Pos(2,2),Pos(1,2)), MoveBuild(0,Pos(3,2),Pos(4,2)), MoveBuild(0,Pos(3,2),Pos(3,1)), MoveBuild(0,Pos(3,2),Pos(4,1)), MoveBuild(0,Pos(3,2),Pos(2,1)), MoveBuild(0,Pos(3,2),Pos(3,3)), MoveBuild(0,Pos(3,2),Pos(4,3)), MoveBuild(0,Pos(3,2),Pos(2,3)), MoveBuild(0,Pos(3,2),Pos(2,2)), MoveBuild(0,Pos(2,4),Pos(3,4)), MoveBuild(0,Pos(2,4),Pos(2,3)), MoveBuild(0,Pos(2,4),Pos(3,3)), MoveBuild(0,Pos(2,4),Pos(1,3)), MoveBuild(0,Pos(2,4),Pos(1,4)), MoveBuild(0,Pos(3,4),Pos(4,4)), MoveBuild(0,Pos(3,4),Pos(3,3)), MoveBuild(0,Pos(3,4),Pos(4,3)), MoveBuild(0,Pos(3,4),Pos(2,3)), MoveBuild(0,Pos(3,4),Pos(2,4)), MoveBuild(0,Pos(1,4),Pos(2,4)), MoveBuild(0,Pos(1,4),Pos(1,3)), MoveBuild(0,Pos(1,4),Pos(2,3)), MoveBuild(0,Pos(1,4),Pos(0,3)), MoveBuild(0,Pos(1,4),Pos(0,4)), MoveBuild(0,Pos(1,3),Pos(2,3)), MoveBuild(0,Pos(1,3),Pos(1,2)), MoveBuild(0,Pos(1,3),Pos(2,2)), MoveBuild(0,Pos(1,3),Pos(0,2)), MoveBuild(0,Pos(1,3),Pos(1,4)), MoveBuild(0,Pos(1,3),Pos(2,4)), MoveBuild(0,Pos(1,3),Pos(0,4)), MoveBuild(0,Pos(1,3),Pos(0,3)), MoveBuild(1,Pos(2,1),Pos(3,1)), MoveBuild(1,Pos(2,1),Pos(2,0)), MoveBuild(1,Pos(2,1),Pos(3,0)), MoveBuild(1,Pos(2,1),Pos(1,0)), MoveBuild(1,Pos(2,1),Pos(2,2)), MoveBuild(1,Pos(2,1),Pos(3,2)), MoveBuild(1,Pos(2,1),Pos(1,2)), MoveBuild(1,Pos(2,1),Pos(1,1)), MoveBuild(1,Pos(1,0),Pos(2,0)), MoveBuild(1,Pos(1,0),Pos(1,1)), MoveBuild(1,Pos(1,0),Pos(2,1)), MoveBuild(1,Pos(1,0),Pos(0,0)), MoveBuild(1,Pos(2,0),Pos(3,0)), MoveBuild(1,Pos(2,0),Pos(2,1)), MoveBuild(1,Pos(2,0),Pos(3,1)), MoveBuild(1,Pos(2,0),Pos(1,1)), MoveBuild(1,Pos(2,0),Pos(1,0)), MoveBuild(1,Pos(0,0),Pos(1,0)), MoveBuild(1,Pos(0,0),Pos(1,1)), MoveBuild(1,Pos(2,2),Pos(3,2)), MoveBuild(1,Pos(2,2),Pos(2,1)), MoveBuild(1,Pos(2,2),Pos(3,1)), MoveBuild(1,Pos(2,2),Pos(1,1)), MoveBuild(1,Pos(2,2),Pos(3,3)), MoveBuild(1,Pos(2,2),Pos(1,3)), MoveBuild(1,Pos(2,2),Pos(1,2)), MoveBuild(1,Pos(0,2),Pos(1,2)), MoveBuild(1,Pos(0,2),Pos(1,1)), MoveBuild(1,Pos(0,2),Pos(0,3)), MoveBuild(1,Pos(0,2),Pos(1,3))),true)



    val restricted = WarFogAnalysis.restrictOppoScope(observed, previousState, previousAction, previousOppoScope)

    restricted should be(Set(Set(Pos(0, 1), Pos(4, 4))))


  }

  it should "restrict oppo scope with history event 5" in {

    val observed = WondevState(5,Map(Pos(0,2) -> 1, Pos(0,0) -> 1, Pos(4,0) -> 0, Pos(3,4) -> 3, Pos(3,1) -> 0, Pos(4,1) -> 0, Pos(2,0) -> 2, Pos(0,3) -> 1, Pos(4,4) -> 0, Pos(3,0) -> 0, Pos(1,1) -> 3, Pos(1,4) -> 0, Pos(0,4) -> 0, Pos(3,2) -> 1, Pos(1,3) -> 0, Pos(2,2) -> 0, Pos(4,2) -> 1, Pos(2,4) -> 2, Pos(0,1) -> 0, Pos(3,3) -> 3, Pos(2,3) -> 1, Pos(1,2) -> 4, Pos(2,1) -> 2, Pos(4,3) -> 2, Pos(1,0) -> 1),List(Pos(2,3), Pos(1,0), Pos(-1,-1), Pos(-1,-1)),List(MoveBuild(0,Pos(2,2),Pos(3,2)), MoveBuild(0,Pos(2,2),Pos(2,1)), MoveBuild(0,Pos(2,2),Pos(3,1)), MoveBuild(0,Pos(2,2),Pos(1,1)), MoveBuild(0,Pos(2,2),Pos(2,3)), MoveBuild(0,Pos(2,2),Pos(3,3)), MoveBuild(0,Pos(2,2),Pos(1,3)), MoveBuild(0,Pos(3,2),Pos(4,2)), MoveBuild(0,Pos(3,2),Pos(3,1)), MoveBuild(0,Pos(3,2),Pos(4,1)), MoveBuild(0,Pos(3,2),Pos(2,1)), MoveBuild(0,Pos(3,2),Pos(3,3)), MoveBuild(0,Pos(3,2),Pos(4,3)), MoveBuild(0,Pos(3,2),Pos(2,3)), MoveBuild(0,Pos(3,2),Pos(2,2)), MoveBuild(0,Pos(2,4),Pos(3,4)), MoveBuild(0,Pos(2,4),Pos(2,3)), MoveBuild(0,Pos(2,4),Pos(3,3)), MoveBuild(0,Pos(2,4),Pos(1,3)), MoveBuild(0,Pos(2,4),Pos(1,4)), MoveBuild(0,Pos(1,4),Pos(2,4)), MoveBuild(0,Pos(1,4),Pos(1,3)), MoveBuild(0,Pos(1,4),Pos(2,3)), MoveBuild(0,Pos(1,4),Pos(0,3)), MoveBuild(0,Pos(1,4),Pos(0,4)), MoveBuild(0,Pos(1,3),Pos(2,3)), MoveBuild(0,Pos(1,3),Pos(2,2)), MoveBuild(0,Pos(1,3),Pos(0,2)), MoveBuild(0,Pos(1,3),Pos(1,4)), MoveBuild(0,Pos(1,3),Pos(2,4)), MoveBuild(0,Pos(1,3),Pos(0,4)), MoveBuild(0,Pos(1,3),Pos(0,3)), MoveBuild(1,Pos(2,0),Pos(3,0)), MoveBuild(1,Pos(2,0),Pos(2,1)), MoveBuild(1,Pos(2,0),Pos(3,1)), MoveBuild(1,Pos(2,0),Pos(1,1)), MoveBuild(1,Pos(2,0),Pos(1,0)), MoveBuild(1,Pos(2,1),Pos(3,1)), MoveBuild(1,Pos(2,1),Pos(2,0)), MoveBuild(1,Pos(2,1),Pos(3,0)), MoveBuild(1,Pos(2,1),Pos(1,0)), MoveBuild(1,Pos(2,1),Pos(2,2)), MoveBuild(1,Pos(2,1),Pos(3,2)), MoveBuild(1,Pos(2,1),Pos(1,1)), MoveBuild(1,Pos(0,1),Pos(1,1)), MoveBuild(1,Pos(0,1),Pos(0,0)), MoveBuild(1,Pos(0,1),Pos(1,0)), MoveBuild(1,Pos(0,1),Pos(0,2)), MoveBuild(1,Pos(0,0),Pos(1,0)), MoveBuild(1,Pos(0,0),Pos(0,1)), MoveBuild(1,Pos(0,0),Pos(1,1))),true)



    val previousAction = PushBuild(1,Pos(1,1),Pos(0,2))

    val previousOppoScope = Set(Set(Pos(1,1), Pos(4,4)))

    val previousState = WondevState(5,Map(Pos(0,2) -> 1, Pos(0,0) -> 1, Pos(4,0) -> 0, Pos(3,4) -> 3, Pos(3,1) -> 0, Pos(4,1) -> 0, Pos(2,0) -> 2, Pos(0,3) -> 1, Pos(4,4) -> 0, Pos(3,0) -> 0, Pos(1,1) -> 2, Pos(1,4) -> 0, Pos(0,4) -> 0, Pos(3,2) -> 1, Pos(1,3) -> 0, Pos(2,2) -> 0, Pos(4,2) -> 1, Pos(2,4) -> 2, Pos(0,1) -> 0, Pos(3,3) -> 2, Pos(2,3) -> 1, Pos(1,2) -> 4, Pos(2,1) -> 2, Pos(4,3) -> 2, Pos(1,0) -> 1),List(Pos(3,3), Pos(1,0), Pos(1,1), Pos(4,4)),List(MoveBuild(0,Pos(4,3),Pos(4,2)), MoveBuild(0,Pos(4,3),Pos(3,2)), MoveBuild(0,Pos(4,3),Pos(3,4)), MoveBuild(0,Pos(4,3),Pos(3,3)), MoveBuild(0,Pos(3,2),Pos(4,2)), MoveBuild(0,Pos(3,2),Pos(3,1)), MoveBuild(0,Pos(3,2),Pos(4,1)), MoveBuild(0,Pos(3,2),Pos(2,1)), MoveBuild(0,Pos(3,2),Pos(3,3)), MoveBuild(0,Pos(3,2),Pos(4,3)), MoveBuild(0,Pos(3,2),Pos(2,3)), MoveBuild(0,Pos(3,2),Pos(2,2)), MoveBuild(0,Pos(4,2),Pos(4,1)), MoveBuild(0,Pos(4,2),Pos(3,1)), MoveBuild(0,Pos(4,2),Pos(4,3)), MoveBuild(0,Pos(4,2),Pos(3,3)), MoveBuild(0,Pos(4,2),Pos(3,2)), MoveBuild(0,Pos(2,2),Pos(3,2)), MoveBuild(0,Pos(2,2),Pos(2,1)), MoveBuild(0,Pos(2,2),Pos(3,1)), MoveBuild(0,Pos(2,2),Pos(2,3)), MoveBuild(0,Pos(2,2),Pos(3,3)), MoveBuild(0,Pos(2,2),Pos(1,3)), MoveBuild(0,Pos(3,4),Pos(3,3)), MoveBuild(0,Pos(3,4),Pos(4,3)), MoveBuild(0,Pos(3,4),Pos(2,3)), MoveBuild(0,Pos(3,4),Pos(2,4)), MoveBuild(0,Pos(2,4),Pos(3,4)), MoveBuild(0,Pos(2,4),Pos(2,3)), MoveBuild(0,Pos(2,4),Pos(3,3)), MoveBuild(0,Pos(2,4),Pos(1,3)), MoveBuild(0,Pos(2,4),Pos(1,4)), MoveBuild(0,Pos(2,3),Pos(3,3)), MoveBuild(0,Pos(2,3),Pos(2,2)), MoveBuild(0,Pos(2,3),Pos(3,2)), MoveBuild(0,Pos(2,3),Pos(2,4)), MoveBuild(0,Pos(2,3),Pos(3,4)), MoveBuild(0,Pos(2,3),Pos(1,4)), MoveBuild(0,Pos(2,3),Pos(1,3)), MoveBuild(1,Pos(2,0),Pos(3,0)), MoveBuild(1,Pos(2,0),Pos(2,1)), MoveBuild(1,Pos(2,0),Pos(3,1)), MoveBuild(1,Pos(2,0),Pos(1,0)), MoveBuild(1,Pos(2,1),Pos(3,1)), MoveBuild(1,Pos(2,1),Pos(2,0)), MoveBuild(1,Pos(2,1),Pos(3,0)), MoveBuild(1,Pos(2,1),Pos(1,0)), MoveBuild(1,Pos(2,1),Pos(2,2)), MoveBuild(1,Pos(2,1),Pos(3,2)), MoveBuild(1,Pos(0,1),Pos(0,0)), MoveBuild(1,Pos(0,1),Pos(1,0)), MoveBuild(1,Pos(0,1),Pos(0,2)), MoveBuild(1,Pos(0,0),Pos(1,0)), MoveBuild(1,Pos(0,0),Pos(0,1)), PushBuild(1,Pos(1,1),Pos(2,2)), PushBuild(1,Pos(1,1),Pos(0,2))),true)


    val restricted = WarFogAnalysis.restrictOppoScope(observed, previousState, previousAction, previousOppoScope)

    restricted should be(Set(Set(Pos(0, 2), Pos(4, 4))))


  }

  it should "restrict oppo scope with history event 6" in {

    val observed = WondevState(5,Map(Pos(0,2) -> 3, Pos(0,0) -> 1, Pos(4,0) -> 0, Pos(3,4) -> 3, Pos(3,1) -> 0, Pos(4,1) -> 0, Pos(2,0) -> 2, Pos(0,3) -> 1, Pos(4,4) -> 0, Pos(3,0) -> 1, Pos(1,1) -> 3, Pos(1,4) -> 2, Pos(0,4) -> 1, Pos(3,2) -> 1, Pos(1,3) -> 4, Pos(2,2) -> 2, Pos(4,2) -> 1, Pos(2,4) -> 3, Pos(0,1) -> 0, Pos(3,3) -> 4, Pos(2,3) -> 2, Pos(1,2) -> 4, Pos(2,1) -> 2, Pos(4,3) -> 2, Pos(1,0) -> 1),List(Pos(2,3), Pos(1,1), Pos(1,4), Pos(-1,-1)),List(MoveBuild(0,Pos(2,2),Pos(3,2)), MoveBuild(0,Pos(2,2),Pos(2,1)), MoveBuild(0,Pos(2,2),Pos(3,1)), MoveBuild(0,Pos(2,2),Pos(2,3)), MoveBuild(0,Pos(3,2),Pos(4,2)), MoveBuild(0,Pos(3,2),Pos(3,1)), MoveBuild(0,Pos(3,2),Pos(4,1)), MoveBuild(0,Pos(3,2),Pos(2,1)), MoveBuild(0,Pos(3,2),Pos(4,3)), MoveBuild(0,Pos(3,2),Pos(2,3)), MoveBuild(0,Pos(3,2),Pos(2,2)), MoveBuild(0,Pos(2,4),Pos(3,4)), MoveBuild(0,Pos(2,4),Pos(2,3)), MoveBuild(0,Pos(3,4),Pos(4,4)), MoveBuild(0,Pos(3,4),Pos(4,3)), MoveBuild(0,Pos(3,4),Pos(2,3)), MoveBuild(0,Pos(3,4),Pos(2,4)), MoveBuild(1,Pos(2,1),Pos(3,1)), MoveBuild(1,Pos(2,1),Pos(2,0)), MoveBuild(1,Pos(2,1),Pos(3,0)), MoveBuild(1,Pos(2,1),Pos(1,0)), MoveBuild(1,Pos(2,1),Pos(2,2)), MoveBuild(1,Pos(2,1),Pos(3,2)), MoveBuild(1,Pos(2,1),Pos(1,1)), MoveBuild(1,Pos(1,0),Pos(2,0)), MoveBuild(1,Pos(1,0),Pos(1,1)), MoveBuild(1,Pos(1,0),Pos(2,1)), MoveBuild(1,Pos(1,0),Pos(0,1)), MoveBuild(1,Pos(1,0),Pos(0,0)), MoveBuild(1,Pos(2,0),Pos(3,0)), MoveBuild(1,Pos(2,0),Pos(2,1)), MoveBuild(1,Pos(2,0),Pos(3,1)), MoveBuild(1,Pos(2,0),Pos(1,1)), MoveBuild(1,Pos(2,0),Pos(1,0)), MoveBuild(1,Pos(0,0),Pos(1,0)), MoveBuild(1,Pos(0,0),Pos(0,1)), MoveBuild(1,Pos(0,0),Pos(1,1)), MoveBuild(1,Pos(2,2),Pos(3,2)), MoveBuild(1,Pos(2,2),Pos(2,1)), MoveBuild(1,Pos(2,2),Pos(3,1)), MoveBuild(1,Pos(2,2),Pos(1,1)), MoveBuild(1,Pos(0,2),Pos(0,1)), MoveBuild(1,Pos(0,2),Pos(1,1)), MoveBuild(1,Pos(0,2),Pos(0,3)), MoveBuild(1,Pos(0,1),Pos(1,1)), MoveBuild(1,Pos(0,1),Pos(0,0)), MoveBuild(1,Pos(0,1),Pos(1,0)), MoveBuild(1,Pos(0,1),Pos(0,2)), PushBuild(0,Pos(1,4),Pos(0,4))),true)




    val previousAction = PushBuild(0,Pos(1,4),Pos(0,4))

    val previousOppoScope = Set(Set(Pos(1,4), Pos(4,4)))

    val previousState = WondevState(5,Map(Pos(0,2) -> 3, Pos(0,0) -> 1, Pos(4,0) -> 0, Pos(3,4) -> 3, Pos(3,1) -> 0, Pos(4,1) -> 0, Pos(2,0) -> 2, Pos(0,3) -> 1, Pos(4,4) -> 0, Pos(3,0) -> 1, Pos(1,1) -> 3, Pos(1,4) -> 1, Pos(0,4) -> 1, Pos(3,2) -> 1, Pos(1,3) -> 4, Pos(2,2) -> 2, Pos(4,2) -> 1, Pos(2,4) -> 3, Pos(0,1) -> 0, Pos(3,3) -> 4, Pos(2,3) -> 2, Pos(1,2) -> 4, Pos(2,1) -> 2, Pos(4,3) -> 2, Pos(1,0) -> 1),List(Pos(2,3), Pos(1,1), Pos(1,4), Pos(-1,-1)),List(MoveBuild(0,Pos(2,2),Pos(3,2)), MoveBuild(0,Pos(2,2),Pos(2,1)), MoveBuild(0,Pos(2,2),Pos(3,1)), MoveBuild(0,Pos(2,2),Pos(2,3)), MoveBuild(0,Pos(3,2),Pos(4,2)), MoveBuild(0,Pos(3,2),Pos(3,1)), MoveBuild(0,Pos(3,2),Pos(4,1)), MoveBuild(0,Pos(3,2),Pos(2,1)), MoveBuild(0,Pos(3,2),Pos(4,3)), MoveBuild(0,Pos(3,2),Pos(2,3)), MoveBuild(0,Pos(3,2),Pos(2,2)), MoveBuild(0,Pos(2,4),Pos(3,4)), MoveBuild(0,Pos(2,4),Pos(2,3)), MoveBuild(0,Pos(3,4),Pos(4,4)), MoveBuild(0,Pos(3,4),Pos(4,3)), MoveBuild(0,Pos(3,4),Pos(2,3)), MoveBuild(0,Pos(3,4),Pos(2,4)), MoveBuild(1,Pos(2,1),Pos(3,1)), MoveBuild(1,Pos(2,1),Pos(2,0)), MoveBuild(1,Pos(2,1),Pos(3,0)), MoveBuild(1,Pos(2,1),Pos(1,0)), MoveBuild(1,Pos(2,1),Pos(2,2)), MoveBuild(1,Pos(2,1),Pos(3,2)), MoveBuild(1,Pos(2,1),Pos(1,1)), MoveBuild(1,Pos(1,0),Pos(2,0)), MoveBuild(1,Pos(1,0),Pos(1,1)), MoveBuild(1,Pos(1,0),Pos(2,1)), MoveBuild(1,Pos(1,0),Pos(0,1)), MoveBuild(1,Pos(1,0),Pos(0,0)), MoveBuild(1,Pos(2,0),Pos(3,0)), MoveBuild(1,Pos(2,0),Pos(2,1)), MoveBuild(1,Pos(2,0),Pos(3,1)), MoveBuild(1,Pos(2,0),Pos(1,1)), MoveBuild(1,Pos(2,0),Pos(1,0)), MoveBuild(1,Pos(0,0),Pos(1,0)), MoveBuild(1,Pos(0,0),Pos(0,1)), MoveBuild(1,Pos(0,0),Pos(1,1)), MoveBuild(1,Pos(2,2),Pos(3,2)), MoveBuild(1,Pos(2,2),Pos(2,1)), MoveBuild(1,Pos(2,2),Pos(3,1)), MoveBuild(1,Pos(2,2),Pos(1,1)), MoveBuild(1,Pos(0,2),Pos(0,1)), MoveBuild(1,Pos(0,2),Pos(1,1)), MoveBuild(1,Pos(0,2),Pos(0,3)), MoveBuild(1,Pos(0,1),Pos(1,1)), MoveBuild(1,Pos(0,1),Pos(0,0)), MoveBuild(1,Pos(0,1),Pos(1,0)), MoveBuild(1,Pos(0,1),Pos(0,2)), PushBuild(0,Pos(1,4),Pos(0,4))),true)



    val restricted = WarFogAnalysis.restrictOppoScope(observed, previousState, previousAction, previousOppoScope)

    restricted should be(Set(Set(Pos(1, 4), Pos(4, 4))))


  }

  it should "restrict oppo scope with history event 7" in {

    val observed = WondevState(5,Map(Pos(0,2) -> 2, Pos(0,0) -> 0, Pos(4,0) -> 4, Pos(3,4) -> 2, Pos(3,1) -> 4, Pos(4,1) -> 3, Pos(2,0) -> 4, Pos(0,3) -> 1, Pos(4,4) -> 3, Pos(3,0) -> 4, Pos(1,1) -> 3, Pos(1,4) -> 0, Pos(0,4) -> 0, Pos(3,2) -> 4, Pos(1,3) -> 1, Pos(2,2) -> 4, Pos(4,2) -> 4, Pos(2,4) -> 1, Pos(0,1) -> 3, Pos(3,3) -> 4, Pos(2,3) -> 4, Pos(1,2) -> 4, Pos(2,1) -> 3, Pos(4,3) -> 1, Pos(1,0) -> 3),List(Pos(3,4), Pos(0,3), Pos(-1,-1), Pos(-1,-1)),List(MoveBuild(0,Pos(4,4),Pos(4,3)), MoveBuild(0,Pos(4,4),Pos(3,4)), MoveBuild(0,Pos(4,3),Pos(4,4)), MoveBuild(0,Pos(4,3),Pos(3,4)), MoveBuild(0,Pos(2,4),Pos(3,4)), MoveBuild(0,Pos(2,4),Pos(1,3)), MoveBuild(0,Pos(2,4),Pos(1,4)), MoveBuild(1,Pos(1,3),Pos(0,2)), MoveBuild(1,Pos(1,3),Pos(1,4)), MoveBuild(1,Pos(1,3),Pos(2,4)), MoveBuild(1,Pos(1,3),Pos(0,4)), MoveBuild(1,Pos(1,3),Pos(0,3)), MoveBuild(1,Pos(0,2),Pos(0,1)), MoveBuild(1,Pos(0,2),Pos(1,1)), MoveBuild(1,Pos(0,2),Pos(0,3)), MoveBuild(1,Pos(0,2),Pos(1,3)), MoveBuild(1,Pos(0,4),Pos(1,4)), MoveBuild(1,Pos(0,4),Pos(0,3)), MoveBuild(1,Pos(0,4),Pos(1,3)), MoveBuild(1,Pos(1,4),Pos(2,4)), MoveBuild(1,Pos(1,4),Pos(1,3)), MoveBuild(1,Pos(1,4),Pos(0,3)), MoveBuild(1,Pos(1,4),Pos(0,4))),true)


    val previousAction = MoveBuild(1,Pos(0,3),Pos(0,2))

    val previousOppoScope = Set(Set(Pos(0,0), Pos(4,1)))

    val previousState = WondevState(5,Map(Pos(0,2) -> 1, Pos(0,0) -> 0, Pos(4,0) -> 4, Pos(3,4) -> 2, Pos(3,1) -> 4, Pos(4,1) -> 3, Pos(2,0) -> 4, Pos(0,3) -> 1, Pos(4,4) -> 3, Pos(3,0) -> 4, Pos(1,1) -> 3, Pos(1,4) -> 0, Pos(0,4) -> 0, Pos(3,2) -> 4, Pos(1,3) -> 1, Pos(2,2) -> 4, Pos(4,2) -> 4, Pos(2,4) -> 1, Pos(0,1) -> 3, Pos(3,3) -> 4, Pos(2,3) -> 4, Pos(1,2) -> 4, Pos(2,1) -> 3, Pos(4,3) -> 1, Pos(1,0) -> 3),List(Pos(3,4), Pos(0,2), Pos(-1,-1), Pos(-1,-1)),List(MoveBuild(0,Pos(4,4),Pos(4,3)), MoveBuild(0,Pos(4,4),Pos(3,4)), MoveBuild(0,Pos(4,3),Pos(4,4)), MoveBuild(0,Pos(4,3),Pos(3,4)), MoveBuild(0,Pos(2,4),Pos(3,4)), MoveBuild(0,Pos(2,4),Pos(1,3)), MoveBuild(0,Pos(2,4),Pos(1,4)), MoveBuild(1,Pos(0,3),Pos(1,3)), MoveBuild(1,Pos(0,3),Pos(0,2)), MoveBuild(1,Pos(0,3),Pos(0,4)), MoveBuild(1,Pos(0,3),Pos(1,4)), MoveBuild(1,Pos(1,3),Pos(0,2)), MoveBuild(1,Pos(1,3),Pos(1,4)), MoveBuild(1,Pos(1,3),Pos(2,4)), MoveBuild(1,Pos(1,3),Pos(0,4)), MoveBuild(1,Pos(1,3),Pos(0,3))),true)




    val restricted = WarFogAnalysis.restrictOppoScope(observed, previousState, previousAction, previousOppoScope)

    restricted should be(Set(Set(Pos(4, 1), Pos(0, 0))))


  }

  it should "restrict oppo scope with history event 8" in {

    val observed = WondevState(5,Map(Pos(0,2) -> 2, Pos(0,0) -> 0, Pos(4,0) -> 4, Pos(3,4) -> 2, Pos(3,1) -> 4, Pos(4,1) -> 3, Pos(2,0) -> 4, Pos(0,3) -> 1, Pos(4,4) -> 3, Pos(3,0) -> 4, Pos(1,1) -> 3, Pos(1,4) -> 0, Pos(0,4) -> 0, Pos(3,2) -> 4, Pos(1,3) -> 2, Pos(2,2) -> 4, Pos(4,2) -> 4, Pos(2,4) -> 1, Pos(0,1) -> 3, Pos(3,3) -> 4, Pos(2,3) -> 4, Pos(1,2) -> 4, Pos(2,1) -> 3, Pos(4,3) -> 1, Pos(1,0) -> 3),List(Pos(3,4), Pos(0,1), Pos(-1,-1), Pos(0,0)),List(MoveBuild(0,Pos(4,4),Pos(4,3)), MoveBuild(0,Pos(4,4),Pos(3,4)), MoveBuild(0,Pos(4,3),Pos(4,4)), MoveBuild(0,Pos(4,3),Pos(3,4)), MoveBuild(0,Pos(2,4),Pos(3,4)), MoveBuild(0,Pos(2,4),Pos(1,3)), MoveBuild(0,Pos(2,4),Pos(1,4)), MoveBuild(1,Pos(1,1),Pos(2,1)), MoveBuild(1,Pos(1,1),Pos(1,0)), MoveBuild(1,Pos(1,1),Pos(0,2)), MoveBuild(1,Pos(1,1),Pos(0,1)), MoveBuild(1,Pos(1,0),Pos(1,1)), MoveBuild(1,Pos(1,0),Pos(2,1)), MoveBuild(1,Pos(1,0),Pos(0,1)), MoveBuild(1,Pos(0,2),Pos(0,1)), MoveBuild(1,Pos(0,2),Pos(1,1)), MoveBuild(1,Pos(0,2),Pos(0,3)), MoveBuild(1,Pos(0,2),Pos(1,3))),true)



    val previousAction = MoveBuild(1,Pos(0,1),Pos(0,0))

    val previousOppoScope = Set(Set(Pos(0,0), Pos(4,1)))

    val previousState = WondevState(5,Map(Pos(0,2) -> 2, Pos(0,0) -> 0, Pos(4,0) -> 4, Pos(3,4) -> 2, Pos(3,1) -> 4, Pos(4,1) -> 3, Pos(2,0) -> 4, Pos(0,3) -> 1, Pos(4,4) -> 3, Pos(3,0) -> 4, Pos(1,1) -> 3, Pos(1,4) -> 0, Pos(0,4) -> 0, Pos(3,2) -> 4, Pos(1,3) -> 2, Pos(2,2) -> 4, Pos(4,2) -> 4, Pos(2,4) -> 1, Pos(0,1) -> 3, Pos(3,3) -> 4, Pos(2,3) -> 4, Pos(1,2) -> 4, Pos(2,1) -> 3, Pos(4,3) -> 1, Pos(1,0) -> 3),List(Pos(3,4), Pos(0,2), Pos(-1,-1), Pos(-1,-1)),List(MoveBuild(0,Pos(4,4),Pos(4,3)), MoveBuild(0,Pos(4,4),Pos(3,4)), MoveBuild(0,Pos(4,3),Pos(4,4)), MoveBuild(0,Pos(4,3),Pos(3,4)), MoveBuild(0,Pos(2,4),Pos(3,4)), MoveBuild(0,Pos(2,4),Pos(1,3)), MoveBuild(0,Pos(2,4),Pos(1,4)), MoveBuild(1,Pos(0,1),Pos(1,1)), MoveBuild(1,Pos(0,1),Pos(0,0)), MoveBuild(1,Pos(0,1),Pos(1,0)), MoveBuild(1,Pos(0,1),Pos(0,2)), MoveBuild(1,Pos(1,1),Pos(2,1)), MoveBuild(1,Pos(1,1),Pos(1,0)), MoveBuild(1,Pos(1,1),Pos(0,0)), MoveBuild(1,Pos(1,1),Pos(0,2)), MoveBuild(1,Pos(1,1),Pos(0,1)), MoveBuild(1,Pos(0,3),Pos(1,3)), MoveBuild(1,Pos(0,3),Pos(0,2)), MoveBuild(1,Pos(0,3),Pos(0,4)), MoveBuild(1,Pos(0,3),Pos(1,4)), MoveBuild(1,Pos(1,3),Pos(0,2)), MoveBuild(1,Pos(1,3),Pos(1,4)), MoveBuild(1,Pos(1,3),Pos(2,4)), MoveBuild(1,Pos(1,3),Pos(0,4)), MoveBuild(1,Pos(1,3),Pos(0,3))),true)





    val restricted = WarFogAnalysis.restrictOppoScope(observed, previousState, previousAction, previousOppoScope)

    restricted should be(Set(Set(Pos(4, 1), Pos(0, 0))))


  }






}
