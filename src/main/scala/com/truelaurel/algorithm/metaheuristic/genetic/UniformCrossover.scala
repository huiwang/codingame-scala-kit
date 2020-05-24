package com.truelaurel.algorithm.metaheuristic.genetic

import com.truelaurel.math.Mathl

/**
  * Created by hwang on 27/05/2017.
  */
object UniformCrossover {

  def crossover[T](
      v: Vector[T],
      w: Vector[T],
      p: Double
  ): (Vector[T], Vector[T]) = {
    require(v.size == w.size, "two vectors should have the same size")
    v.zip(w)
      .map(pair => if (p >= Mathl.random.nextDouble()) pair.swap else pair)
      .unzip
  }

}
