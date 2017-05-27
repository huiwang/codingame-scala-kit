package com.truelaurel.tests

import com.truelaurel.tests.Compilation.compiles
import org.scalatest.{FlatSpec, Matchers}

class CompilationTest extends FlatSpec with Matchers {
  behavior of "Compilation"

  it should "compile valid code" in {
    compiles("object Toto extends App { println() }") shouldBe true
  }

  it should "reject invalid code" in {
    compiles("object Toto extends App { println) }") shouldBe false
  }


}
