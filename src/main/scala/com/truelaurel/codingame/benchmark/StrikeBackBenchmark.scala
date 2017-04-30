package com.truelaurel.codingame.benchmark

import java.util.concurrent.TimeUnit

import com.truelaurel.codingame.csb.head.StrikeBackPlayer
import com.truelaurel.codingame.csb.model.{CheckPoint, Pod, StrikeBackContext, StrikeBackState}
import com.truelaurel.codingame.time.CountStopper
import com.truelaurel.codingame.vectorial.Vectorl
import org.openjdk.jmh.annotations._

/**
  * Created by hwang on 30/04/2017.
  */
@State(Scope.Benchmark)
class StrikeBackBenchmark {
  val state = ???


  @Benchmark
  @BenchmarkMode(Array(Mode.AverageTime))
  @OutputTimeUnit(TimeUnit.MILLISECONDS)
  def benchmark(): Unit = {
    val player = StrikeBackPlayer(StrikeBackContext.me, StrikeBackContext.other, new CountStopper(100))
    player.reactTo(state)
  }
}
