package com.truelaurel.algorithm.metaheuristic.tweak

import com.truelaurel.math.Mathl

/**
  * Created by hwang on 12/02/2017.
  */
object NoiseGenerators {

  type G = () => Double

  def uniform(halfRange: Double, center: Double = 0): G =
    () => center + Mathl.randomBetween(-halfRange, halfRange)

  def gaussian(mean: Double, stdDerivation: Double): G =
    () => mean + stdDerivation * Mathl.random.nextGaussian()

}
