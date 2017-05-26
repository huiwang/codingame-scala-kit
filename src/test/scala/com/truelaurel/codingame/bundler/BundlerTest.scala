package com.truelaurel.codingame.bundler

import java.io.File

import com.truelaurel.codingame.bundle.{Bundler, BundlerIo}
import org.scalactic._
import org.scalatest.{FlatSpec, Matchers}

class BundlerTest extends FlatSpec with Matchers {

  behavior of "Bundler"


  it should "copy simple content" in {
    val inputName = "Demo.scala"
    val content = "object Demo extends App"
    val io = prepareMockIo(Map(inputName -> content))
    val output = Bundler(inputName, io).buildOutput
    output shouldBe content
  }

  it should "strip multiline comments" in {
    val inputName = "Demo.scala"
    val content =
      """object Demo extends App {
        |/*
        |comment out
        |*/
        |println("hello")
        |}""".stripMargin
    val io = prepareMockIo(Map(inputName -> content))
    val output = Bundler(inputName, io).buildOutput
    output should equal(
      """object Demo extends App {
        |
        |println("hello")
        |}""".stripMargin)(after being linefeedNormalised)
  }


  it should "copy also file in same folder" in {
    val inputName = "pkg/Demo.scala"
    val content = "object Demo extends App"
    val utilName = "pkg/Util.scala"
    val utilContent = "object Util { def abs(x) = if(x>0) x else -x }"
    val io = prepareMockIo(Map(
      inputName -> content,
      utilName -> utilContent))
    val output = Bundler("Demo.scala", io).buildOutput
    output shouldBe content + "\n" + utilContent
  }


  it should "resolve import from another package" in {
    val inputName = "Demo.scala"
    val content =
      """import util.Util
        |object Demo extends App""".stripMargin
    val utilName = "util/Util.scala"
    val utilContent =
      """package util
        |object Util { def abs(x) = if(x>0) x else -x }""".stripMargin
    val io = prepareMockIo(Map(
      inputName -> content,
      utilName -> utilContent))
    val output = Bundler(inputName, io).buildOutput
    output shouldBe
      "object Util { def abs(x) = if(x>0) x else -x }\n" +
        "object Demo extends App"
  }


  private def prepareMockIo(fileContents: Map[String, String]): BundlerIo = new BundlerIo {
    val root = new File(".")

    val contentsByFile = fileContents.map {
      case (pathName, content) => new File(root, pathName) -> content
    }

    def filesInFolder(folder: File): List[File] =
      contentsByFile.keys
        .toList
        .filter(_.getParentFile == folder)

    def findFolder(packageElements: Array[String]): File =
      packageElements.init.foldLeft(root) {
        case (folder, pkg) => new File(folder, pkg)
      }

    def findFile(fileName: String): File =
      new File(root, fileContents.keys.find(fn => fn.endsWith(fileName)).get)

    def save(fileName: String, content: String): Unit = ???

    def readFile(file: File): List[String] = {
      val content = contentsByFile(file)
      content.split("""\r\n|\n""").toList
    }
  }


  def findFolder(packageElements: Array[String]): File = {
    packageElements.foldLeft(new File(".")) {
      case (folder, pkg) => new File(folder, pkg)
    }
  }


  //  it should "have same output for GhostInTheCell" in {
  //    checkOuput(
  //      "src/main/scala/com/truelaurel/codingame/ghostcell/online/GhostInTheCell.scala",
  //      "GhostInTheCell.scala")
  //  }
  //
  //  it should "have same output for CodersCaribbean" in {
  //    val startFile = "src/main/scala/com/truelaurel/codingame/caribbean/online/CodersCaribbean.scala"
  //    val expectedOutput = "CodersCaribbean.scala"
  //    checkOuput(startFile, expectedOutput)
  //  }
  //
  //  it should "have same output for GhostMain" in {
  //    val startFile = "src/main/scala/com/tyrcho/GhostMain.scala"
  //    val expectedOutput = "GhostMain.scala"
  //    checkOuput(startFile, expectedOutput)
  //  }
  //
  //  it should "have same output for CoderStrikeBack" in {
  //    val startFile = "src/main/scala/com/truelaurel/codingame/csb/io/CoderStrikeBack.scala"
  //    val expectedOutput = "CoderStrikeBack.scala"
  //    checkOuput(startFile, expectedOutput)
  //  }
  //
  //  private def checkOuput(startFile: String, shortName: String): Unit = {
  //    val expectedContent = Source.fromFile("src/test/resources/com/truelaurel/codingame/bundler/" + shortName).getLines.mkString("\n")
  //    val output = Bundler(shortName, StdBundlerIo()).buildOutput
  //
  //    output.split("\n") should contain theSameElementsAs expectedContent.split("\n")
  //  }

  def linefeedNormalised: Uniformity[String] =
    new AbstractStringUniformity {
      def normalized(s: String): String = s.replaceAll("\r\n", "\n")

      override def toString: String = "linefeedNormalised"
    }

}