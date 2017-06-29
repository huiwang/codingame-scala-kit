package com.truelaurel.samplegames.wondev.io

import com.truelaurel.math.geometry.Pos
import com.truelaurel.samplegames.wondev.domain.FastContext
import org.scalatest.{FlatSpec, Matchers}

class FastControllerTest extends FlatSpec with Matchers {

  val rows = Seq(
    "...0...",
    "..122..",
    ".01121.",
    "0101412",
    ".02330.",
    "..211..",
    "...0...")

  "FastController" should "guess next state" in {
    val guessed = FastController(7).guessState(FastContext(7, 2), rows, Seq(Pos(3, 3), Pos(3, 4)), Seq(Pos(-1, -1), Pos(-1, -1)))
    val possibleOps = Array(3, 9, 10, 11, 15, 19, 21, 22, 26, 27, 29, 33, 45)
    guessed.possibleOpUnits shouldBe Array(possibleOps, possibleOps)

    val rows2 = Seq(
      "...0...",
      "..123..",
      ".01121.",
      "0101412",
      ".02330.",
      "..211..",
      "...0...")

    val context2 = FastContext(7, 2, Some(guessed.copy(possibleOpUnits = Array(Array(3, 9), Array(15)))))
    val guessed2 = FastController(7).guessState(context2, rows2, Seq(Pos(3, 3), Pos(3, 4)), Seq(Pos(-1, -1), Pos(-1, -1)))
    guessed2.possibleOpUnits shouldBe Array(Array(3), Array(15))
    guessed2.opUnits shouldBe Array(3, 15)
  }
}
