package com.truelaurel.samplegames.wondev.benchmark

import com.truelaurel.math.geometry.Pos
import com.truelaurel.samplegames.wondev.domain._
import com.truelaurel.samplegames.wondev.strategy.MinimaxPlayer
import org.openjdk.jmh.annotations._

/**
  * jmh:run -prof jmh.extras.JFR -i 1 -wi 1 -f1 -t1 WondevBenchmark
  */
@State(Scope.Benchmark)
class WondevBenchmark {

  val player = MinimaxPlayer
  val state = ???
  @Benchmark
  def wondevMinimax(): Unit = {
    player.react(state)
  }

}
