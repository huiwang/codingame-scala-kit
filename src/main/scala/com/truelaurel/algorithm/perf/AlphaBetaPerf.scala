package com.truelaurel.algorithm.perf

import com.truelaurel.algorithm.alphabeta.AlphaBetaAi
import com.truelaurel.math.geometry.Pos
import com.truelaurel.samplegames.dummy.{DummyBoard, DummyRules}
import com.truelaurel.samplegames.gomoku.GomokuRules
import org.openjdk.jmh.annotations._

import scala.util.Random

@State(Scope.Benchmark)
class AlphaBetaPerf {
  val rules = DummyRules(20, 10)
  val rules74 = GomokuRules(7, 4)
  val rules33 = GomokuRules(3, 3)

  @Benchmark
  def dummyDepth3(): Int = {
    AlphaBetaAi(rules, heuristic).chooseMove(rules.initial, 3)
  }

  @Benchmark
  def dummyDepth5(): Int = {
    AlphaBetaAi(rules, heuristic).chooseMove(rules.initial, 5)
  }

  def heuristic(s: DummyBoard): Double = Random.nextDouble

  @Benchmark
  def gomoku74Depth2(): Pos = {
    AlphaBetaAi(rules74, rules74.centerHeuristic).chooseMove(rules74.initial, 2)
  }

  @Benchmark
  def gomoku33Depth9(): Pos = {
    AlphaBetaAi(rules33, rules33.centerHeuristic).chooseMove(rules33.initial, 9)
  }
}
