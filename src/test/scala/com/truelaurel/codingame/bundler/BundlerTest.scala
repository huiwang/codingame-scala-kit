package com.truelaurel.codingame.bundler

import java.io.File

import com.truelaurel.codingame.bundle.Bundler
import com.truelaurel.codingame.bundle.Bundler.args
import com.truelaurel.codingame.caribbean.common.{CollisionAnalysis, Ship}
import com.truelaurel.codingame.hexagons.Offset
import org.scalatest.{FlatSpec, Matchers}

import scala.io.Source

/**
  * Created by tyrcho on 07/05/2017.
  */
class BundlerTest extends FlatSpec with Matchers {

  behavior of "Bundler"

  it should "have same output for GhostInTheCell" in {
    val startFile = "src/main/scala/com/truelaurel/codingame/ghostcell/online/GhostInTheCell.scala"
    val expectedOutput = "target/GhostInTheCell.scala"
    checkOuput(startFile, expectedOutput)
  }

  def checkOuput(fileName: String, expectedFileName: String) = {
    val expected = Source.fromFile(expectedFileName).getLines().toList.mkString(System.lineSeparator)
    val output = new Bundler(fileName).buildOutput(new File(fileName))

    output shouldBe expected
  }


}