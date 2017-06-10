package ai.scala.fp.game.mcts.perf


import ai.scala.fp.game.gomoku.{GomokuBoard, GomokuRules}
import ai.scala.fp.game.mcts.MctsNode
import ai.scala.fp.game.mcts.perf.Dummy._
import ai.scala.fp.geo.Pos
import org.openjdk.jmh.annotations._

@State(Scope.Benchmark)
class MctsPerf {

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
    dummyRoot.steps(100)
  }

  val rules3 = GomokuRules(3, 3)
  val gomoku3 = MctsNode(rules3.initial, rules3, rules3.randomPlay)
  val rules7 = GomokuRules(7, 5)
  val gomoku7 = MctsNode(rules7.initial, rules7, rules7.randomPlay)
}
