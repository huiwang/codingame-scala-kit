package com.truelaurel.codingame.bundler

import java.io.File

import com.truelaurel.codingame.bundle.{Bundler, BundlerIo}
import com.truelaurel.tests.Compilation._
import org.scalactic._
import org.scalatest.{FlatSpec, Matchers}

class BundlerTest extends FlatSpec with Matchers {
  behavior of "Bundler"

  it should "copy simple content" in {
    //GIVEN
    val inputName = "Demo.scala"
    val content = "object Demo extends App"
    val io = prepareMockIo(Map(inputName -> content))
    //WHEN
    val output = Bundler(inputName, io).buildOutput
    //THEN
    output shouldBe content
    compiles(output) shouldBe true
  }

  it should "keep scala imports" in {
    //GIVEN
    val inputName = "Demo.scala"
    val content =
      """import scala.io.Source._
        |object Demo extends App""".stripMargin
    val io = prepareMockIo(Map(inputName -> content))
    //WHEN
    val output = Bundler(inputName, io).buildOutput
    //THEN
    output should equal(content)(after being linefeedNormalised)
    compiles(output) shouldBe true
  }

  it should "strip multiline comments" in {
    //GIVEN
    val inputName = "Demo.scala"
    val content =
      """object Demo extends App {
        |/*
        |comment out
        |*/
        |println("hello")
        |}""".stripMargin
    val io = prepareMockIo(Map(inputName -> content))
    //WHEN
    val output = Bundler(inputName, io).buildOutput
    //THEN
    output should equal(
      """object Demo extends App {
        |
        |println("hello")
        |}""".stripMargin)(after being linefeedNormalised)
    compiles(output) shouldBe true
  }


  it should "copy also file in same folder" in {
    //GIVEN
    val inputName = "pkg/Demo.scala"
    val content = "object Demo extends App"
    val utilName = "pkg/Util.scala"
    val utilContent = "object Util { def abs(x:Int) = if(x>0) x else -x }"
    val io = prepareMockIo(Map(
      inputName -> content,
      utilName -> utilContent))
    //WHEN
    val output = Bundler("Demo.scala", io).buildOutput
    //THEN
    output shouldBe content + "\n" + utilContent
    compiles(output) shouldBe true
  }


  it should "keep import Util._" in {
    //GIVEN
    val inputName = "pkg/Demo.scala"
    val content =
      """import util.Util._
        |object Demo extends App""".stripMargin
    val utilName = "util/Util.scala"
    val utilContent = "object Util { def abs(x:Int) = if(x>0) x else -x }"
    val io = prepareMockIo(Map(
      inputName -> content,
      utilName -> utilContent))
    //WHEN
    val output = Bundler("Demo.scala", io).buildOutput
    //THEN
    val expected =
      """object Util { def abs(x:Int) = if(x>0) x else -x }
        |import Util._
        |object Demo extends App""".stripMargin
    output should equal(expected)(after being linefeedNormalised)
    compiles(output) shouldBe true
  }


  it should "resolve import from another package" in {
    //GIVEN
    val inputName = "Demo.scala"
    val content =
      """import util.Util
        |object Demo extends App""".stripMargin
    val utilName = "util/Util.scala"
    val utilContent =
      """package util
        |object Util { def abs(x:Int) = if(x>0) x else -x }""".stripMargin
    val io = prepareMockIo(Map(
      inputName -> content,
      utilName -> utilContent))
    //WHEN
    val output = Bundler(inputName, io).buildOutput
    //THEN
    output shouldBe
      "object Util { def abs(x:Int) = if(x>0) x else -x }\n" +
        "object Demo extends App"
    compiles(output) shouldBe true
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


  private def linefeedNormalised: Uniformity[String] =
    new AbstractStringUniformity {
      def normalized(s: String): String = s.replaceAll("\r\n", "\n")

      override def toString: String = "linefeedNormalised"
    }


}