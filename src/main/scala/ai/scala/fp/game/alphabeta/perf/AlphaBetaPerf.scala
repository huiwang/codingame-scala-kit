package ai.scala.fp.game.alphabeta.perf

import ai.scala.fp.game.alphabeta.AlphaBetaAi
import ai.scala.fp.game.gomoku.GomokuRules
import ai.scala.fp.game.{DummyBoard, DummyRules}
import ai.scala.fp.geo.Pos
import org.openjdk.jmh.annotations._

import scala.util.Random

@State(Scope.Benchmark)
class AlphaBetaPerf {
  val rules = DummyRules(20, 10)

  def heuristic(s: DummyBoard): Double = Random.nextDouble

  @Benchmark
  def dummyDepth3(): Int = {
    AlphaBetaAi(rules, heuristic).chooseMove(rules.initial, 3)
  }

  @Benchmark
  def dummyDepth5(): Int = {
    AlphaBetaAi(rules, heuristic).chooseMove(rules.initial, 5)
  }

  val rules74 = GomokuRules(7, 4)
  val rules33 = GomokuRules(3,3)

  @Benchmark
  def gomoku74Depth2(): Pos = {
    AlphaBetaAi(rules74, rules74.centerHeuristic).chooseMove(rules74.initial, 2)
  }

  @Benchmark
  def gomoku33Depth9(): Pos = {
    AlphaBetaAi(rules33, rules33.centerHeuristic).chooseMove(rules33.initial, 9)
  }
}
