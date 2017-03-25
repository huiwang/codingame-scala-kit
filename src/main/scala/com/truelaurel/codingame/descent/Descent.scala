package com.truelaurel.codingame.descent

import scala.io.StdIn


/**
  * Created by hwang on 25/03/2017.
  */
object Player {

  def main(args: Array[String]): Unit = {
    while (true) {
      val mountains = 0.until(8).map(_ => StdIn.readInt())
      println(mountains.indexWhere(_ == mountains.max))
    }
  }
}
