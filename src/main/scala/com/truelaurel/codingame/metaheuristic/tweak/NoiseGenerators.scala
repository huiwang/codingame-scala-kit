package com.truelaurel.codingame.metaheuristic.tweak

import com.truelaurel.codingame.math.Mathl

/**
  * Created by hwang on 12/02/2017.
  */
object NoiseGenerators {

  type G = () => Double

  def uniform(halfRange: Double, center : Double = 0): G = () => center + Mathl.randomBetween(-halfRange, halfRange)

  def gaussian(mean: Double, variance: Double): G = () => mean + Math.sqrt(variance) * Mathl.random.nextGaussian()

}
