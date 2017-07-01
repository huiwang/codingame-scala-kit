package com.truelaurel.samplegames.wondev.io

import com.truelaurel.math.geometry.Pos
import com.truelaurel.math.geometry.grid.FastGrid
import com.truelaurel.samplegames.wondev.domain.{FastContext, FastState}
import org.scalatest.{FlatSpec, Matchers}

class FastControllerTest extends FlatSpec with Matchers {
  val s = 3
  val grid = FastGrid(s)

  val rows = Seq(
    "000",
    "000",
    "000")

  val controller = FastController(s)

  val hiddenPos = Pos(-1, -1)
  val oneHidden = Seq(hiddenPos)
  val twoHidden = Seq(hiddenPos, hiddenPos)

  "FastController for 1 unit" should "guess opponent positions (initially)" in {
    val guessedState = controller.guessState(FastContext(s, 1), rows, Seq(Pos(0, 0)), oneHidden)

    val possibleOps = Seq(Pos(0, 2), Pos(1, 2), Pos(2, 2), Pos(2, 1), Pos(2, 0))
    guessedState.possibleOpUnits(0).map(grid.pos) should contain theSameElementsAs possibleOps
  }

  val rows2 = Seq(
    "000",
    "001",
    "000")
  it should "guess opponent positions (after 1 move)" in {
    val guessedState = controller.guessState(FastContext(s, 1), rows, Seq(Pos(1, 0)), oneHidden)
    guessedState.possibleOpUnits(0).map(grid.pos) should contain theSameElementsAs
      Seq(Pos(0, 2), Pos(1, 2), Pos(2, 2))

    val context2 = FastContext(s, 1, Some(guessedState))
    val guessed2 = controller.guessState(context2, rows2, Seq(Pos(0, 1)), oneHidden)

    guessed2.possibleOpUnits(0).map(grid.pos) should contain theSameElementsAs
      Seq(Pos(2, 2))

  }

  "FastController for 2 units" should "guess opponent positions (initially)" in {
    val guessedState = controller.guessState(FastContext(s, 2), rows, Seq(Pos(0, 0), Pos(0, 1)), twoHidden)

    val possibleOps = Seq(Pos(2, 2), Pos(2, 1), Pos(2, 0))
    guessedState.possibleOpUnits(0).map(grid.pos) should contain theSameElementsAs possibleOps
    guessedState.possibleOpUnits(1).map(grid.pos) should contain theSameElementsAs possibleOps
  }

  it should "guess opponent positions (knowing one)" in {
    val guessedState = controller.guessState(
      FastContext(s, 2), rows,
      myUnits = Seq(Pos(0, 0), Pos(0, 1)),
      opUnits = Seq(hiddenPos, Pos(2, 1)))

    guessedState.possibleOpUnits(0).map(grid.pos) should contain theSameElementsAs Seq(Pos(2, 2), Pos(2, 0))
    guessedState.possibleOpUnits(1).map(grid.pos) should contain theSameElementsAs Seq(Pos(2, 1))
    guessedState.opUnits(1) shouldBe grid.pos(Pos(2, 1))
  }

  it should "remember opponent position (only one can move)" in {
    val guessedState = controller.guessState(
      FastContext(s, 2), rows,
      myUnits = Seq(Pos(0, 0), Pos(0, 1)),
      opUnits = Seq(hiddenPos, Pos(2, 2)))

    guessedState.possibleOpUnits(0).map(grid.pos) should contain theSameElementsAs Seq(Pos(2, 1), Pos(2, 0))
    guessedState.opUnits(1) shouldBe grid.pos(Pos(2, 2))

    val guessedState2 = controller.guessState(
      FastContext(s, 2, Some(guessedState)), rows2,
      myUnits = Seq(Pos(0, 0), Pos(0, 1)),
      opUnits = Seq(Pos(1, 2), hiddenPos))
    guessedState2.opUnits(0) shouldBe grid.pos(Pos(1, 2))
    guessedState2.possibleOpUnits(0).map(grid.pos) should contain theSameElementsAs Seq(Pos(1, 2))
    guessedState2.possibleOpUnits(1).map(grid.pos) should contain theSameElementsAs Seq(Pos(2, 2))
  }

  it should "guess opponent position (bug in CG)" in {
    val size = 5
    val grid = FastGrid(size)
    val rows = Seq(
      "21311",
      "10410",
      "00330",
      "00003",
      "01030")

    val lastState = FastState(size, myUnits = Seq(Pos(1, 1), Pos(1, 3)), opUnits = Seq(Pos(2, 2), Pos(3, 3)))
      .copy(heights = FastController.parseHeights(rows, grid))

    val rows2 = Seq(
      "21311",
      "10420",
      "00330",
      "00003",
      "01030")
    val guessedState = FastController(size).guessState(FastContext(size, 1, Some(lastState)), rows2, myUnits = Seq(Pos(1, 1), Pos(1, 3)), opUnits = twoHidden)
    guessedState.possibleOpUnits(0).map(grid.pos) should contain theSameElementsAs Seq(Pos(3, 2))
    guessedState.possibleOpUnits(1).map(grid.pos) should contain theSameElementsAs Seq(Pos(3, 3))
  }
}
