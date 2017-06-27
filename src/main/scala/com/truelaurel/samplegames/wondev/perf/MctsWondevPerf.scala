package com.truelaurel.samplegames.wondev.perf

import com.truelaurel.algorithm.mcts.MctsNode
import com.truelaurel.samplegames.wondev.domain.WondevRules
import org.openjdk.jmh.annotations._

//  jmh:run -i 3 -wi 3 -f1 -t1 MctsWondevPerf

@State(Scope.Benchmark)
class MctsWondevPerf {

  val rules = WondevRules(5)
  val node = MctsNode(rules.initial, rules, rules.randomPlay)

  @Benchmark
  def wondev5in100steps() = {
    node.steps(100)
  }


}
