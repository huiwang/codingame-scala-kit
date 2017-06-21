package com.truelaurel.algorithm.perf

import com.truelaurel.algorithm.mcts.MctsNode
import com.truelaurel.math.geometry.Pos
import com.truelaurel.samplegames.dummy.DummyBoard
import com.truelaurel.samplegames.gomoku.{GomokuBoard, GomokuRules}
import org.openjdk.jmh.annotations._

@State(Scope.Benchmark)
class MctsPerf {

  val rules3 = GomokuRules(3, 3)
  val gomoku3 = MctsNode(rules3.initial, rules3, rules3.randomPlay)
  val rules7 = GomokuRules(7, 5)
  val gomoku7 = MctsNode(rules7.initial, rules7, rules7.randomPlay)

  @Benchmark
  def gomoku3in100steps(): MctsNode[GomokuBoard, Pos] = {
    gomoku3.steps(100)
  }

  @Benchmark
  def gomoku7in100steps(): MctsNode[GomokuBoard, Pos] = {
    gomoku7.steps(100)
  }

  @Benchmark
  def dummy100steps(): MctsNode[DummyBoard, Int] = {
    DummyMcts.dummyRoot.steps(100)
  }
}
