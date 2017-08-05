package com.truelaurel.algorithm.perf

import com.truelaurel.algorithm.mcts.MctsNode
import com.truelaurel.samplegames.gomoku.GomokuRules
import org.openjdk.jmh.annotations._

@State(Scope.Benchmark)
class MctsPerf {

  val rules3 = GomokuRules(3, 3)
  val gomoku3 = new MctsNode(rules3.initial, rules3, rules3.randomPlay)
  val rules7 = GomokuRules(7, 5)
  val gomoku7 = new MctsNode(rules7.initial, rules7, rules7.randomPlay)

  @Benchmark
  def gomoku3in100steps() = {
    gomoku3.steps(100)
  }

  @Benchmark
  def gomoku7in100steps() = {
    gomoku7.steps(100)
  }

  @Benchmark
  def dummy100steps() = {
    DummyMcts.dummyRoot.steps(100)
  }
}
