package com.truelaurel.codingame.tool.bundle

object BundlerMain {
  def main(args: Array[String]): Unit = {
    if (args.isEmpty) {
      println("Input file name must be provided")
      sys.exit(1)
    }
    args.foreach { fileName =>
      Bundler(fileName, StdBundlerIo()).bundle()
    }
  }
}
