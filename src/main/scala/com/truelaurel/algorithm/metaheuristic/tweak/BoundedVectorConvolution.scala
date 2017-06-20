package com.truelaurel.algorithm.metaheuristic.tweak

import com.truelaurel.math.Mathl

/**
  * Created by hwang on 12/02/2017.
  */
class BoundedVectorConvolution(noiseProbability: Double, min: Double, max: Double) {

  def tweak(v: Vector[Double], noiseGenerator: () => Double): Vector[Double] = {
    v.map(elem => {
      if (noiseProbability >= Mathl.random.nextDouble()) {
        var tweaked = elem
        do {
          tweaked = noiseGenerator.apply() + elem
        } while (tweaked < min || tweaked > max)
        tweaked
      } else {
        elem
      }
    })
  }

}
