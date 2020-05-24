package com.truelaurel.algorithm.dichotomy

object Dichotomy {

  /**
    * Find a target value between the lower and upper bound where we can't go lower anymore
    *
    * @param low   lower bound
    * @param high  upper bound
    * @param lower evaluation function guiding if we should go lower by taking the lower half interval
    * @return the target value where we can't go lower anymore
    */
  def search(low: Int, high: Int, lower: Int => Boolean): Int = {
    if (low == high) low
    else {
      val mid = (low + high) / 2
      if (lower(mid)) {
        search(low, mid, lower)
      } else {
        search(mid + 1, high, lower)
      }
    }
  }
}
