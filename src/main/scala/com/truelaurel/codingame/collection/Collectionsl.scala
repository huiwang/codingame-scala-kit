package com.truelaurel.codingame.collection

/**
  * Created by hwang on 25/12/2016.
  */
object Collectionsl {

  def adjust[K, V](map: Map[K, V])(key: K)(mapper: V => V): Map[K, V] = {
    map.get(key) match {
      case Some(v) => map.updated(key, mapper(v))
      case None => map
    }
  }

}
