package com.truelaurel.samplegames.wondev

sealed trait Action

case class LegalAction(actionType: String, unit: Int, dir1: String, dir2: String) extends Action {
  override val toString = s"$actionType $unit $dir1 $dir2"
}