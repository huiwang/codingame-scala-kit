package com.truelaurel.samplegames.wondev

import scala.io.StdIn.{readInt, readLine}

object WondevState {
  def read(context: WondevContext) = {
    import context._

    for (i <- 0 until size) {
      val row = readLine
    }
    for (i <- 0 until unitsperplayer) {
      val Array(unitx, unity) = for (i <- readLine split " ") yield i.toInt
    }
    for (i <- 0 until unitsperplayer) {
      val Array(otherx, othery) = for (i <- readLine split " ") yield i.toInt
    }
    val legalactions = readInt
    for (i <- 0 until legalactions) {
      val Array(t, _index, dir1, dir2) = readLine split " "
      val index = _index.toInt
    }
    WondevState(0)
  }
}

case class WondevState(turn: Int)