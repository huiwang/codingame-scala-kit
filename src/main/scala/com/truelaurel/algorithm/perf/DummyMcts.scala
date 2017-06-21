package com.truelaurel.algorithm.perf

import com.truelaurel.algorithm.mcts.MctsNode
import com.truelaurel.samplegames.dummy.DummyRules

object DummyMcts {
  type DummyMove = Int
  val rules = DummyRules()
  val dummyRoot = MctsNode(rules.initial, rules, rules.randomPlay)
}
