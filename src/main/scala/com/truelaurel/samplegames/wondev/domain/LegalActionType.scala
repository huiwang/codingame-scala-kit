package com.truelaurel.samplegames.wondev.domain

import com.truelaurel.math.geometry.Direction

sealed trait LegalActionType


case object Build extends LegalActionType

case object Push extends LegalActionType

case class LegalAction(actionType: LegalActionType, unitIndex: Int, dir1: Direction, dir2: Direction)