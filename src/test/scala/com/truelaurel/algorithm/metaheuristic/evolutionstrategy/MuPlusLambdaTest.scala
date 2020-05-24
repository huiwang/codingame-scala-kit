package com.truelaurel.algorithm.metaheuristic.evolutionstrategy

import com.truelaurel.time.CountStopper
import org.scalatest.{FlatSpec, Matchers}

class MuPlusLambdaTest extends FlatSpec with Matchers {

  behavior of "MuPlusLambda"

  val muSize = 20
  val popSize = 100
  val stopper = new CountStopper(1000)

  val mpl = new MuPlusLambda(muSize, popSize, stopper)

  it can "search for a good angle to throw a rock far" in {
    val result = mpl.search[RockThrowingSolution](new RockThrowingProblem)
    (result.degree - 45.0) should be < 1.0
  }

}
