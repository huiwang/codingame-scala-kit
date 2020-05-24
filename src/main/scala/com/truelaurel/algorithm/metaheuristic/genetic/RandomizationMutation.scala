package com.truelaurel.algorithm.metaheuristic.genetic

import com.truelaurel.math.Mathl

/**
  * Created by hwang on 27/05/2017.
  */
object RandomizationMutation {

  /**
    * @param v vector to be mutated
    * @param randomLegalElem function that provides a random legal elem
    * @param p mutation probability, usually lower than 1/l where l is the length of input vector
    * @tparam T type of element
    * @return randomly mutated vector
    */
  def mutate[T](
      v: Vector[T],
      randomLegalElem: Int => T,
      p: Double
  ): Vector[T] = {
    v.zipWithIndex.map(pair =>
      if (p >= Mathl.random.nextDouble()) randomLegalElem(pair._2) else pair._1
    )
  }
}
