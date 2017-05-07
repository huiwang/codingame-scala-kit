package com.truelaurel.codingame.bundler

import java.io.File

import com.truelaurel.codingame.bundle.Bundler
import com.truelaurel.codingame.bundle.Bundler.args
import com.truelaurel.codingame.caribbean.common.{CollisionAnalysis, Ship}
import com.truelaurel.codingame.hexagons.Offset
import org.scalatest.{Assertion, FlatSpec, Matchers}

import scala.io.Source

/**
  * Created by tyrcho on 07/05/2017.
  */
class BundlerTest extends FlatSpec with Matchers {

  behavior of "Bundler"

  it should "have same output for GhostInTheCell" in {
    checkOuput(
      "src/main/scala/com/truelaurel/codingame/ghostcell/online/GhostInTheCell.scala",
      "GhostInTheCell.scala")
  }

  it should "have same output for CodersCaribbean" in {
    val startFile = "src/main/scala/com/truelaurel/codingame/caribbean/online/CodersCaribbean.scala"
    val expectedOutput = "CodersCaribbean.scala"
    checkOuput(startFile, expectedOutput)
  }

  it should "have same output for GhostMain" in {
    val startFile ="src/main/scala/com/tyrcho/GhostMain.scala"
    val expectedOutput = "GhostMain.scala"
    checkOuput(startFile, expectedOutput)
  }

  it should "have same output for CoderStrikeBack" in {
    val startFile ="src/main/scala/com/truelaurel/codingame/csb/io/CoderStrikeBack.scala"
    val expectedOutput = "CoderStrikeBack.scala"
    checkOuput(startFile, expectedOutput)
  }

  def checkOuput(startFile: String, shortName: String): Unit = {
    val expectedContent = Source.fromFile("src/test/resources/com/truelaurel/codingame/bundler/" + shortName).getLines.mkString("\n")
    val output = new Bundler(shortName).buildOutput

    output shouldBe expectedContent
  }


}