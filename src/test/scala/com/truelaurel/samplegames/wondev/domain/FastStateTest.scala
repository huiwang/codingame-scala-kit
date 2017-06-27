package com.truelaurel.samplegames.wondev.domain

import com.truelaurel.math.geometry.grid.FastGrid
import com.truelaurel.math.geometry.{Pos, _}
import org.scalatest.{FlatSpec, Matchers}

class FastStateTest extends FlatSpec with Matchers {

  import FastState._

  val grid = FastGrid(5)

  val myPos0 = Pos(1, 1)
  val myPos1 = Pos(1, 2)
  val opPos0 = Pos(2, 4)
  val opPos1 = Pos(1, 3)

  // .....
  // .T...
  // .T...
  // .F...
  // ..F..

  val state = FastState(5,
    myUnits = Vector(grid.pos(myPos0), grid.pos(myPos1)),
    opUnits = Vector(grid.pos(opPos0), grid.pos(opPos1)))

  "fast state : build" should "increase height" in {
    val p = grid.pos(Pos(2, 2))
    state.heights(p) shouldBe 0

    val built = state.build(p)

    built.heights(p) shouldBe 1
  }

  "fast state : move" should "move my unit" in {
    val moved = state.move(0, N)

    moved.myUnits(0) shouldBe grid.pos(myPos0.neighborIn(N))
  }

  "fast state : push" should "push op unit" in {
    val pushed = state.push(1, S, SW)

    pushed.myUnits(1) shouldBe grid.pos(myPos1)
    pushed.opUnits(1) shouldBe grid.pos(opPos1.neighborIn(SW))
  }

  "fast state : move" should "move op unit" in {
    val moved = state.copy(nextPlayer = false).move(0, W)

    moved.opUnits(0) shouldBe grid.pos(opPos0.neighborIn(W))
  }


  "fast state : move&build" should "move my unit and build" in {
    val moved = state.applyAction(MoveBuild(0, S, SE))

    moved.myUnits(0) shouldBe grid.pos(myPos0.neighborIn(S))
    moved.heights(grid.pos(Pos(2, 3))) shouldBe 1
  }


  "fast state : push&build" should "push op unit and build" in {
    val moved = state.applyAction(MovePush(1, S, SE))

    moved.myUnits(1) shouldBe grid.pos(myPos1)
    moved.opUnits(1) shouldBe grid.pos(opPos1.neighborIn(SE))
    moved.heights(grid.pos(opPos1)) shouldBe 1
  }

  "fast state : pass" should "change active player" in {
    val next = state.applyAction(Pass)

    next.nextPlayer shouldBe false
  }

  "fast state : move&build" should "increase score" in {
    val higher = state.heights
      .updated(grid.pos(myPos0), SCORE_HEIGHT)
      .updated(grid.pos(myPos0.neighborIn(N)), SCORE_HEIGHT)
    val s = state.copy(heights = higher)
    val next = s.applyAction(MoveBuild(0, N, S))

    next.myScore shouldBe 1
  }

  "fast state" should "list valid move actions for 1 unit" in {
    val moves = for {
      moveDir <- Direction.all diff Set(N, S)
      buildDir <- Direction.all
      builtPos = myPos1.neighborIn(moveDir).neighborIn(buildDir)
      builtP = grid.pos(builtPos)
      if grid.isValid(builtP)
      if !Set(myPos0, opPos0, opPos1).contains(builtPos)
    } yield MoveBuild(1, moveDir, buildDir)

    state.moveActions(1).toSeq.map(_.toString).sorted shouldBe moves.toSeq.map(_.toString).sorted
  }

  "fast state" should "list valid move actions for 1 unit, avoiding inaccessible heights" in {
    val eastTooHigh = myPos1.neighborIn(E)
    val westHole = myPos1.neighborIn(W)
    val higher = state.heights
      .updated(grid.pos(myPos1), SCORE_HEIGHT)
      .updated(grid.pos(eastTooHigh), MAX_BUILT_HEIGHT)
      .updated(grid.pos(westHole), HOLE_HEIGHT)

    val moves = for {
      moveDir <- Direction.all diff Set(N, S, E, W)
      buildDir <- Direction.all
      builtPos = myPos1.neighborIn(moveDir).neighborIn(buildDir)
      builtP = grid.pos(builtPos)
      if grid.isValid(builtP)
      if !Set(myPos0, opPos0, opPos1, eastTooHigh, westHole).contains(builtPos)
    } yield MoveBuild(1, moveDir, buildDir)

    val s = state.copy(heights = higher)

    s.moveActions(1).toSeq.map(_.toString).sorted shouldBe moves.toSeq.map(_.toString).sorted
  }


}
