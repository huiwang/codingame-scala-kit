package com.truelaurel.codingame.time

/**
  * Created by hwang on 20/04/2017.
  */
trait Stopper {

  def start(): Unit

  def willOutOfTime: Boolean
}
