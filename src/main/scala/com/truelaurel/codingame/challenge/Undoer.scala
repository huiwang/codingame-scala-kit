package com.truelaurel.codingame.challenge

import scala.collection.mutable.ArrayBuffer

object Undoer {
  def of(undoers: ArrayBuffer[() => Unit]): () => Unit = {
    () => {
      var i = 0
      while (i < undoers.length) {
        undoers(i)()
        i += 1
      }
    }
  }
}
