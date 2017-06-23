package com.truelaurel.samplegames.wondev

sealed trait Action

case class LegalAction(actionType: String, index: Int, dir1: String, dir2: String) extends Action {
  override val toString = s"$actionType $index $dir1 $dir2"
}