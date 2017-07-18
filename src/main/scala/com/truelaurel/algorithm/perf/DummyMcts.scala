package com.truelaurel.algorithm.perf

import com.truelaurel.algorithm.mcts.MctsNode2p
import com.truelaurel.samplegames.dummy.DummyRules

object DummyMcts {
  type DummyMove = Int
  val rules = DummyRules()
  val dummyRoot = new MctsNode2p(rules.initial, rules, rules.randomPlay)
}
