package com.truelaurel.codingame.metaheuristic.evolutionstrategy

import java.util.concurrent.TimeUnit

import com.truelaurel.codingame.metaheuristic.model.{Problem, Solution}
import org.scalatest.{FlatSpec, Matchers}

import scala.concurrent.duration.Duration

/**
  * Created by hwang on 17/04/2017.
  */
class MuPlusLambdaTest extends FlatSpec with Matchers {

  behavior of "MuPlusLambdaTest"

  it should "search" in {
    val lambda = new MuPlusLambda(2, 6, Duration(10, TimeUnit.MILLISECONDS))
    val search = lambda.search(TestProblem())
    search.value should be(10)
  }

}


case class TestProblem() extends Problem[TestSolution] {
  override def randomSolution(): TestSolution = {
    TestSolution(10)
  }

  override def tweakSolution(solution: TestSolution): TestSolution = {
    TestSolution(solution.value)
  }
}

case class TestSolution(value: Int) extends Solution {
  lazy val quality: Double = {
    value
  }
}