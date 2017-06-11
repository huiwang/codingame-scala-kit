package ai.scala.fp.game.alphabeta.perf

import ai.scala.fp.game.alphabeta.AlphaBetaAi
import ai.scala.fp.game.{DummyBoard, DummyRules}
import org.openjdk.jmh.annotations._

import scala.util.Random

@State(Scope.Benchmark)
class AlphaBetaPerf {
  val rules = DummyRules(20, 10)

  @Benchmark
  def dummyDepth3(): Int = {
    AlphaBetaAi(rules, heuristic).chooseMove(rules.initial, 3)
  }

  @Benchmark
  def dummyDepth5(): Int = {
    AlphaBetaAi(rules, heuristic).chooseMove(rules.initial, 5)
  }

  def heuristic(s: DummyBoard): Double = Random.nextDouble
}
