package com.truelaurel.algorithm.mcts.perf

import com.truelaurel.algorithm.mcts.perf.DummyMcts._

object MctsDebugPerf {
  def main(args: Array[String]): Unit = {
    dummyRoot.steps(1000000)
  }

}
