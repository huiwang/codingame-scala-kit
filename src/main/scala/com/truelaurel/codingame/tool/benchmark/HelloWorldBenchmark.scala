package com.truelaurel.codingame.tool.benchmark

import org.openjdk.jmh.annotations.Benchmark

// This class is there to check during CI that jmh benchmarks are not broken in SBT.
class HelloWorldBenchmark {

  @Benchmark
  def wellHelloThere(): Unit = {
    // this method was intentionally left blank.
  }

}
