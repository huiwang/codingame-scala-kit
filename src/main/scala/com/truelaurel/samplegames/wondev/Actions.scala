package com.truelaurel.samplegames.wondev

sealed trait Action

case object DummyAction extends Action {
  override val toString = "MOVE&BUILD 0 N S"
}