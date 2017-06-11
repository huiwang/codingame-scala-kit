package ai.scala.fp.game.mcts.perf

import ai.scala.fp.game.DummyRules
import ai.scala.fp.game.mcts.MctsNode

object DummyMcts {
  val rules = DummyRules()
  val dummyRoot = MctsNode(rules.initial, rules, rules.randomPlay)

  type DummyMove = Int
}
