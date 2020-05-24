package com.truelaurel.compilation

import com.truelaurel.compilation.Compilation.compiles
import org.scalatest.{FlatSpec, Matchers}

import scala.tools.reflect.ToolBoxError

class CompilationTest extends FlatSpec with Matchers {
  behavior of "Compilation"

  it should "compile valid code" in {
    compiles("object Toto extends App { println() }") shouldBe true
  }

  it should "reject invalid code" in {
    intercept[ToolBoxError] {
      compiles("object Toto extends App { println) }")
    }
  }

}
