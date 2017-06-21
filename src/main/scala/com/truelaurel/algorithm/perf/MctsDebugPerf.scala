package com.truelaurel.algorithm.perf

object MctsDebugPerf {
  def main(args: Array[String]): Unit = {
    DummyMcts.dummyRoot.steps(1000000)
  }

}
